module Proteome.Grep.Replace where

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text
import Path (parseAbsFile)
import Prelude hiding (group)
import Ribosome (
  Buffer,
  Handler,
  Rpc,
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  ScratchState (ScratchState),
  mapReport,
  pathText,
  resumeReport,
  toMsgpack,
  )
import Ribosome.Api (bufferGetLines, bufferSetLines, bufferSetOption, nvimCommand, vimCallFunction)
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, wipeBuffer)
import Ribosome.Api.Option (withOption)
import Ribosome.Data.FileBuffer (FileBuffer (FileBuffer))
import qualified Ribosome.Data.FloatOptions as FloatBorder
import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions))
import Ribosome.Host.Data.RpcType (group)
import qualified Ribosome.Scratch as Scratch

import qualified Proteome.Data.Env as Env (replace)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.GrepOutputLine as GrepOutputLine
import Proteome.Data.GrepOutputLine (GrepOutputLine (GrepOutputLine))
import Proteome.Data.Replace (Replace (Replace))
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError (BadReplacement, CouldntLoadBuffer))
import Proteome.Data.ReplaceError (ReplaceError)

scratchName :: Text
scratchName =
  "proteome-replace"

replaceBuffer ::
  Members [Scratch, Rpc, AtomicState Env] r =>
  NonEmpty GrepOutputLine ->
  Sem r ()
replaceBuffer lines' = do
  scratch <- Scratch.show content options
  let buffer = Scratch.buffer scratch
  bufferSetOption buffer "buftype" ("acwrite" :: Text)
  bufferAutocmd buffer "BufWriteCmd" def { group = Just "ProteomeReplace" } "silent! ProReplaceSave"
  bufferAutocmd buffer "BufUnload" def { group = Just "ProteomeReplace" } "silent! ProReplaceQuit"
  atomicModify' (#replace ?~ Replace scratch lines')
  where
    content =
      GrepOutputLine.content <$> lines'
    options =
      def {
        Scratch.name = ScratchId scratchName,
        Scratch.modify = True,
        Scratch.focus = True,
        Scratch.filetype = Just scratchName
      }

-- If the deleted line was surrounded by blank lines or buffer edges, there will be extraneous whitespace.
-- First check whether the line number of the deleted line was line 0 and its content is now empty.
-- Then do the same for the last line.
-- Finally, check if both the preceding and current line are empty.
deleteExtraBlankLine ::
  Member Rpc r =>
  Buffer ->
  Int ->
  Sem r ()
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

fileBuffer ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe FileBuffer)
fileBuffer path =
  join <$> traverse bufferForFile (parseAbsFile (toString path))

replaceLine ::
  Members [Rpc, Stop ReplaceError] r =>
  Text ->
  GrepOutputLine ->
  Sem r (Maybe Buffer)
replaceLine updatedLine (GrepOutputLine path line _ _) = do
  exists <- isJust <$> bufferForFile path
  unless exists (addBuffer (pathText path))
  () <- vimCallFunction "bufload" [toMsgpack path]
  FileBuffer buffer _ <- stopNote (ReplaceError.CouldntLoadBuffer path) =<< bufferForFile path
  bufferSetLines buffer line (line + 1) False replacement
  deleteExtraBlankLine buffer line
  pure (bool (Just buffer) Nothing exists)
  where
    replacement =
      [updatedLine | not (Text.null updatedLine)]

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine _ number _ _) =
  -number

replaceFloatOptions :: FloatOptions
replaceFloatOptions =
  FloatOptions def 1 1 0 0 False def Nothing FloatBorder.None True False (Just def) (Just 1)

withReplaceEnv ::
  Members [Rpc !! RpcError, Rpc, Resource] r =>
  Sem r [Maybe Buffer] ->
  Sem r ()
withReplaceEnv run = do
  withOption "hidden" True do
    transient <- run
    resume_ (nvimCommand "noautocmd wall")
    traverse_ wipeBuffer (catMaybes transient)

replaceLines ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  Buffer ->
  [(Text, GrepOutputLine)] ->
  Sem r ()
replaceLines scratchBuffer lines' = do
  bufferSetOption scratchBuffer "buftype" ("nofile" :: Text)
  withReplaceEnv do
    traverse (uncurry replaceLine) (sortOn lineNumberDesc lines')
  bufferSetOption scratchBuffer "buftype" ("acwrite" :: Text)
  bufferSetOption scratchBuffer "modified" False

deleteLines ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  [GrepOutputLine] ->
  Sem r ()
deleteLines lines' =
  withReplaceEnv do
    traverse (uncurry replaceLine) (sortOn lineNumberDesc (zip (repeat "") lines'))

replaceSave ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  Replace ->
  Sem r ()
replaceSave (Replace (ScratchState _ _ buffer _ _ _ _) lines') = do
  updatedLines <- bufferContent buffer
  if length updatedLines /= length lines'
  then badReplacement
  else replaceLines buffer (zip updatedLines (NonEmpty.toList lines'))
  where
    badReplacement =
      stop ReplaceError.BadReplacement

proReplaceSave ::
  Members [AtomicState Env, Rpc !! RpcError, Resource] r =>
  Handler r ()
proReplaceSave =
  resumeReport $ mapReport do
    traverse_ replaceSave =<< atomicGets Env.replace

proReplaceQuit ::
  Member (AtomicState Env) r =>
  Handler r ()
proReplaceQuit =
  atomicModify' (#replace .~ Nothing)
