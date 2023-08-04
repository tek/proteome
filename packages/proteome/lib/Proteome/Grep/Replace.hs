module Proteome.Grep.Replace where

import Control.Monad (foldM)
import Data.List (insertBy)
import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Data.Semigroup (Sum (Sum, getSum))
import qualified Data.Text as Text
import Path (File, SomeBase, fromSomeFile, parseAbsFile)
import Ribosome (
  Buffer,
  Handler,
  Rpc,
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  ScratchState (ScratchState),
  atomic,
  mapReport,
  pathText,
  resumeReport,
  toMsgpack,
  )
import Ribosome.Api (
  bufferGetLines,
  bufferSetLines,
  bufferSetOption,
  nvimBufSetExtmark,
  nvimCallFunction,
  nvimCommand,
  nvimCreateNamespace,
  nvimGetOption,
  vimCallFunction,
  windowSetOption,
  )
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, wipeBuffer)
import Ribosome.Api.Option (withOption)
import Ribosome.Data.FileBuffer (FileBuffer (FileBuffer))
import qualified Ribosome.Data.FloatOptions as FloatBorder
import Ribosome.Float (FloatOptions (FloatOptions), FloatRelative (Editor))
import Ribosome.Host (msgpackMap)
import Ribosome.Host.Data.RpcType (group)
import qualified Ribosome.Scratch as Scratch

import qualified Proteome.Data.Env as Env (replace)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.GrepState as GrepState
import Proteome.Data.GrepState (GrepOutputLine (GrepOutputLine))
import Proteome.Data.Replace (Replace (Replace))
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError (BadReplacement, CouldntLoadBuffer))
import Proteome.Data.ReplaceError (ReplaceError)

scratchName :: Text
scratchName =
  "proteome-replace"

replaceFloatOptions ::
  Int ->
  Int ->
  FloatOptions
replaceFloatOptions totalWidth totalHeight =
  FloatOptions {
    relative = Editor,
    focusable = True,
    noautocmd = True,
    style = Nothing,
    zindex = Just 1,
    ..
  }
  where
    FloatOptions {row = _, col = _, width = _, height = _, ..} = def
    row = margin totalHeight height
    col = margin totalWidth width
    margin t l = fromMaybe 0 ((t - l) `div` 2)
    width = size totalWidth
    height = size totalHeight
    size l = ceiling @Double (fromIntegral l * 0.9)

replaceBuffer ::
  Members [Scratch, Rpc, AtomicState Env] r =>
  NonEmpty GrepOutputLine ->
  Sem r ()
replaceBuffer lines' = do
  width <- nvimGetOption "columns"
  height <- nvimGetOption "lines"
  scratch <- Scratch.show content (options width height)
  let buffer = scratch.buffer
  ns <- nvimCreateNamespace "replace-paths"
  bufferAutocmd buffer "BufWriteCmd" def { group = Just "ProteomeReplace" } "silent! ProReplaceSave"
  bufferAutocmd buffer "BufUnload" def { group = Just "ProteomeReplace" } "silent! ProReplaceQuit"
  () <- atomic do
    bufferSetOption buffer "buftype" ("acwrite" :: Text)
    windowSetOption scratch.window "spell" False
    foldM (addPathLine buffer ns) 0 (Map.toList grouped)
    nvimCallFunction "winrestview" [toMsgpack (Map.fromList [("topfill" :: Text, 1 :: Int64)])]
  atomicModify' (#replace ?~ Replace scratch grouped)
  where
    content = fmap (.content) =<< Map.elems grouped

    grouped :: Map (SomeBase File) [GrepOutputLine]
    grouped = foldl (\ z a -> Map.alter (Just . ins a) a.relative z) mempty lines'

    ins gol = \case
      Nothing -> [gol]
      Just old -> insertBy (comparing (.line)) gol old

    options w h =
      def {
        Scratch.name = ScratchId scratchName,
        Scratch.modify = True,
        Scratch.focus = True,
        Scratch.filetype = Just scratchName,
        Scratch.float = Just float,
        Scratch.maxSize = Just float.height
      }
      where
        float = replaceFloatOptions w h

    addPathLine buf ns index (path, ls) = do
      nvimBufSetExtmark buf ns index 0 (virtLineParams path)
      pure (index + length ls)

    virtLineParams :: SomeBase File -> Map Text Object
    virtLineParams path =
      msgpackMap
        ("virt_lines", [[(toText (fromSomeFile path), "ProteomeReplaceFile")]] :: [[(Text, Text)]])
        ("virt_lines_above", True)

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
replaceLine updatedLine (GrepOutputLine {file, line}) = do
  exists <- isJust <$> bufferForFile file
  unless exists (addBuffer (pathText file))
  () <- vimCallFunction "bufload" [toMsgpack file]
  FileBuffer buffer _ <- stopNote (ReplaceError.CouldntLoadBuffer file) =<< bufferForFile file
  bufferSetLines buffer line (line + 1) False replacement
  deleteExtraBlankLine buffer line
  pure (bool (Just buffer) Nothing exists)
  where
    replacement =
      [updatedLine | not (Text.null updatedLine)]

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine {line}) =
  -line

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
  if length updatedLines /= getSum (foldMap (Sum . length) lines')
  then badReplacement
  else replaceLines buffer (zip updatedLines (join (Map.elems lines')))
  where
    badReplacement =
      stop ReplaceError.BadReplacement

-- TODO quit after saving, controlled by setting
proReplaceSave ::
  Members [AtomicState Env, Rpc !! RpcError, Resource] r =>
  Handler r ()
proReplaceSave =
  resumeReport $ mapReport do
    traverse_ replaceSave =<< atomicGets (.replace)

proReplaceQuit ::
  Member (AtomicState Env) r =>
  Handler r ()
proReplaceQuit =
  atomicModify' (#replace .~ Nothing)
