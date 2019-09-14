module Proteome.Grep.Replace where

import Control.Lens (view)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, closeBuffer, setBufferLine)
import Ribosome.Api.Option (withOption)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.FloatOptions (FloatOptions(FloatOptions))
import Ribosome.Data.Scratch (Scratch(Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch(scratchBuffer))
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus, scratchModify)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Window)
import Ribosome.Nvim.Api.IO (bufferSetOption, nvimWinSetBuf, vimCommand)
import Ribosome.Scratch (createFloat, showInScratch)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (replace)
import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import qualified Proteome.Data.GrepOutputLine as GrepOutputLine (text)
import Proteome.Data.Replace (Replace(Replace))
import Proteome.Data.ReplaceError (ReplaceError)
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError(BadReplacement, CouldntLoadBuffer))

scratchName :: Text
scratchName =
  "proteome-replace"

replaceBuffer ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  NonEmpty (MenuItem GrepOutputLine) ->
  m ()
replaceBuffer items = do
  scratch <- showInScratch text options
  let buffer = Scratch.scratchBuffer scratch
  bufferSetOption buffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferAutocmd buffer "ProteomeReplaceSave" "BufWriteCmd" "silent! ProReplaceSave"
  bufferAutocmd buffer "ProteomeReplaceQuit" "BufUnload" "silent! ProReplaceQuit"
  setL @Env Env.replace (Just (Replace scratch lines'))
  where
    text =
      view GrepOutputLine.text <$> lines'
    lines' =
      view MenuItem.meta <$> items
    options =
      scratchModify . scratchFocus . defaultScratchOptions $ scratchName

replaceLine ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Window ->
  Text ->
  GrepOutputLine ->
  m (Maybe Buffer)
replaceLine window updatedLine (GrepOutputLine path line _ _) = do
  exists <- isJust <$> bufferForFile path
  unless exists $ addBuffer path
  buffer <- hoistMaybe (ReplaceError.CouldntLoadBuffer path) =<< bufferForFile path
  nvimWinSetBuf window buffer
  setBufferLine buffer line updatedLine
  return $ if exists then Nothing else Just buffer

replaceLines ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Buffer ->
  [(Text, GrepOutputLine)] ->
  m ()
replaceLines scratchBuffer lines' = do
  (buffer, window) <- createFloat floatOptions
  transient <- withOption "hidden" True (traverse (uncurry (replaceLine window)) lines')
  bufferSetOption scratchBuffer "buftype" (toMsgpack ("nofile" :: Text))
  ignoreError @RpcError $ vimCommand "noautocmd wall"
  bufferSetOption scratchBuffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferSetOption scratchBuffer "modified" (toMsgpack False)
  traverse_ closeBuffer (catMaybes transient)
  closeWindow window
  closeBuffer buffer
  where
    floatOptions =
      FloatOptions def 1 1 0 0 True def

replaceSave ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Replace ->
  m ()
replaceSave (Replace (Scratch _ buffer _ _ _) lines') = do
  updatedLines <- bufferContent buffer
  if length updatedLines /= length lines'
  then badReplacement
  else replaceLines buffer (zip updatedLines (NonEmpty.toList lines'))
  where
    badReplacement =
      throwHoist ReplaceError.BadReplacement

proReplaceSave ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  m ()
proReplaceSave =
  traverse_ replaceSave =<< getL @Env Env.replace

proReplaceQuit ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  m ()
proReplaceQuit =
  setL @Env Env.replace Nothing
