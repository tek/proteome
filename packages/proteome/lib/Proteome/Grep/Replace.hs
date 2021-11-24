module Proteome.Grep.Replace where

import Control.Lens (view)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, wipeBuffer)
import Ribosome.Api.Option (withOption)
import qualified Ribosome.Data.FloatOptions as FloatBorder
import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions))
import Ribosome.Data.Scratch (Scratch (Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch (scratchBuffer))
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (bufferGetLines, bufferSetLines, bufferSetOption, vimCommand, vimCallFunction)
import Ribosome.Scratch (showInScratch)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (replace)
import Proteome.Data.GrepOutputLine (GrepOutputLine (GrepOutputLine))
import qualified Proteome.Data.GrepOutputLine as GrepOutputLine (content)
import Proteome.Data.Replace (Replace (Replace))
import Proteome.Data.ReplaceError (ReplaceError)
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError (BadReplacement, CouldntLoadBuffer))

scratchName :: Text
scratchName =
  "proteome-replace"

replaceBuffer ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s Env m =>
  NonEmpty GrepOutputLine ->
  m ()
replaceBuffer lines' = do
  scratch <- showInScratch content options
  let buffer = Scratch.scratchBuffer scratch
  bufferSetOption buffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferAutocmd buffer "ProteomeReplaceSave" "BufWriteCmd" "silent! ProReplaceSave"
  bufferAutocmd buffer "ProteomeReplaceQuit" "BufUnload" "silent! ProReplaceQuit"
  setL @Env Env.replace (Just (Replace scratch lines'))
  where
    content =
      view GrepOutputLine.content <$> lines'
    options =
      def {
        _name = scratchName,
        _modify = True,
        _focus = True,
        _filetype = Just scratchName
      }

-- If the deleted line was surrounded by blank lines or buffer edges, there will be extraneous whitespace.
-- First check whether the line number of the deleted line was line 0 and its content is now empty.
-- Then do the same for the last line.
-- Finally, check if both the preceding and current line are empty.
deleteExtraBlankLine ::
  MonadIO m =>
  NvimE e m =>
  Buffer ->
  Int ->
  m ()
deleteExtraBlankLine buffer line = do
  check (line - 2) line [""]
  check line (line + 1) [""]
  check (line - 1) line ["", ""]
  where
    check l r target = do
      content <- readLines l (r + 1)
      when (content == target) delete
    readLines l r =
      bufferGetLines buffer (clamp0 l) r False
    delete =
      bufferSetLines buffer line (line + 1) False []
    clamp0 a | a < 0 = 0
    clamp0 a = a

replaceLine ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  Text ->
  GrepOutputLine ->
  m (Maybe Buffer)
replaceLine updatedLine (GrepOutputLine path line _ _) = do
  exists <- isJust <$> bufferForFile path
  unless exists (addBuffer path)
  () <- vimCallFunction "bufload" [toMsgpack path]
  buffer <- hoistMaybe (ReplaceError.CouldntLoadBuffer path) =<< bufferForFile path
  bufferSetLines buffer line (line + 1) False replacement
  deleteExtraBlankLine buffer line
  pure (bool (Just buffer) Nothing exists)
  where
    replacement =
      if Text.null updatedLine
      then []
      else [updatedLine]

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine _ number _ _) =
  -number

replaceFloatOptions :: FloatOptions
replaceFloatOptions =
  FloatOptions def 1 1 0 0 False def Nothing FloatBorder.None True False (Just def) (Just 1)

withReplaceEnv ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m [Maybe Buffer] ->
  m ()
withReplaceEnv run = do
  withOption "hidden" True do
    transient <- run
    ignoreError @RpcError $ vimCommand "noautocmd wall"
    traverse_ wipeBuffer (catMaybes transient)

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
  bufferSetOption scratchBuffer "buftype" (toMsgpack ("nofile" :: Text))
  withReplaceEnv do
    traverse (uncurry replaceLine) (sortOn lineNumberDesc lines')
  bufferSetOption scratchBuffer "buftype" (toMsgpack ("acwrite" :: Text))
  bufferSetOption scratchBuffer "modified" (toMsgpack False)

deleteLines ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  [GrepOutputLine] ->
  m ()
deleteLines lines' =
  withReplaceEnv do
    traverse (uncurry replaceLine) (sortOn lineNumberDesc (zip (repeat "") lines'))

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
  MonadDeepState s Env m =>
  m ()
proReplaceQuit =
  setL @Env Env.replace Nothing
