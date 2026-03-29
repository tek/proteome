module Proteome.Grep.Replace where

import Control.Monad (foldM)
import Data.List (insertBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Data.MessagePack (Object)
import Data.Semigroup (Sum (Sum, getSum))
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, File, Path, SomeBase, fromSomeFile, parseAbsFile)
import qualified Ribosome as Ribosome
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
  nvimBufIsLoaded,
  nvimBufSetExtmark,
  nvimCallFunction,
  nvimCommand,
  nvimCreateNamespace,
  nvimExecLua,
  nvimGetOption,
  windowSetOption,
  )
import Ribosome.Api.Autocmd (bufferAutocmd)
import Ribosome.Api.Buffer (addBuffer, bufferContent, bufferForFile, fileBuffers, wipeBuffer)
import Ribosome.Api.Option (withOption)
import Ribosome.Data.FileBuffer (FileBuffer (..))
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
import qualified Proteome.Data.ReplaceError as ReplaceError (ReplaceError (BadReplacement, BufferErrors))
import Proteome.Data.ReplaceError (ReplaceError)

scratchName :: Text
scratchName =
  "proteome-replace"

replaceFloatOptions ::
  Int ->
  Int ->
  Int ->
  FloatOptions
replaceFloatOptions totalWidth totalHeight maxSize =
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
    height = min maxSize (size totalHeight)
    size l = ceiling @Double (fromIntegral l * 0.8)

-- TODO add diff indicators via colors
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

    options w h =
      def {
        Scratch.name = ScratchId scratchName,
        Scratch.modify = True,
        Scratch.focus = True,
        Scratch.filetype = Just scratchName,
        Scratch.float = Just float,
        Scratch.resize = False
      }
      where
        float = replaceFloatOptions w h lengthWithVirtLines

    lengthWithVirtLines = length content + 2 * Map.size grouped - 1

    content = fmap (.content) =<< Map.elems grouped

    grouped :: Map (SomeBase File) [GrepOutputLine]
    grouped = foldl (\ z a -> Map.alter (Just . ins a) a.relative z) mempty lines'

    ins gol = \case
      Nothing -> [gol]
      Just old -> insertBy (comparing (.line)) gol old

    addPathLine buf ns index (path, ls) = do
      nvimBufSetExtmark buf ns index 0 (virtLineParams path)
      pure (index + length ls)

    virtLineParams :: SomeBase File -> Map Text Object
    virtLineParams path =
      msgpackMap
        ("virt_lines", [
          [("", "Normal")],
          [
            ("* ", "ProteomeReplaceBullet"),
            (toText (fromSomeFile path), "ProteomeReplaceFile")
          ]
        ] :: [[(Text, Text)]])
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
  Member Rpc r =>
  Buffer ->
  Text ->
  GrepOutputLine ->
  Sem r ()
replaceLine buffer updatedLine GrepOutputLine {line, content}
  | updatedLine == content
  = unit
  | otherwise
  = do
    bufferSetLines buffer line (line + 1) False replacement
    deleteExtraBlankLine buffer line
  where
    replacement = [updatedLine | not (Text.null updatedLine)]

-- | Pre-built index of existing file buffers with their loaded state.
-- Built once before the replace operation to avoid per-file buffer list scans.
type BufferIndex = Map (Path Abs File) (Buffer, Bool)

-- | Build an index of all file buffers with their loaded state.
buildBufferIndex ::
  Members [Rpc !! RpcError, Rpc] r =>
  Sem r BufferIndex
buildBufferIndex = do
  fbs <- fileBuffers
  loaded <- atomic (traverse (nvimBufIsLoaded . (.buffer)) fbs)
  pure (Map.fromList [(fb.path, (fb.buffer, l)) | (fb, l) <- zip fbs loaded])

-- | Load a buffer for a file, using the pre-built index to avoid scanning the buffer list.
-- Returns the buffer and whether it should be wiped after the operation
-- (i.e. it was either not in the list or unloaded before we touched it).
ensureBuffer ::
  Member (Rpc !! RpcError) r =>
  BufferIndex ->
  Path Abs File ->
  Sem r (Either RpcError (Buffer, Bool))
ensureBuffer index file =
  case index !? file of
    Just (buffer, True) -> do
      resumeEither do
        nvimCommand [exon|silent checktime #{pathText file}|]
        pure (buffer, False)
    Just (buffer, False) -> do
      resumeEither do
        load
        pure (buffer, True)
    Nothing ->
      resumingOr (pure . Left) addAndLoad \case
        Just FileBuffer {buffer} -> pure (Right (buffer, True))
        Nothing -> pure (Left "Buffer vanished after loading")
  where
    addAndLoad = do
      addBuffer (pathText file)
      load
      bufferForFile file

    load = Ribosome.noautocmd $ nvimCommand [exon|silent call bufload('#{pathText file}')|]

-- | Write a buffer to disk, then wipe it if it's transient.
-- Returns an error message if the write fails.
writeAndCleanup ::
  Member (Rpc !! RpcError) r =>
  Path Abs File ->
  Buffer ->
  Bool ->
  Sem r (Maybe (Path Abs File, RpcError))
writeAndCleanup file buffer transient = do
  result <- resuming onError $ Ribosome.noautocmd do
    () <- nvimExecLua "vim.api.nvim_buf_call(..., function() vim.cmd('silent write') end)" [toMsgpack buffer]
    pure Nothing
  when transient do
    resume_ (wipeBuffer buffer)
  pure result
  where
    onError err = pure (Just (file, err))

replaceInBuffer ::
  Member (Rpc !! RpcError) r =>
  NonEmpty (Text, GrepOutputLine) ->
  (Buffer, Bool) ->
  Sem r (Maybe (Path Abs File, RpcError))
replaceInBuffer lns@((_, GrepOutputLine {file}) :| _) (buffer, transient) = do
  replaceErr <- resuming (pure . Just . show) $ Ribosome.noautocmd do
    traverse_ (uncurry (replaceLine buffer)) lns
    pure Nothing
  case replaceErr of
    Just err -> do
      when transient (resume_ (wipeBuffer buffer))
      pure (Just (file, err))
    Nothing ->
      writeAndCleanup file buffer transient

replaceLinesInFile ::
  Member (Rpc !! RpcError) r =>
  BufferIndex ->
  NonEmpty (Text, GrepOutputLine) ->
  Sem r (Maybe (Path Abs File, RpcError))
replaceLinesInFile index lns@((_, GrepOutputLine {file}) :| _) =
  ensureBuffer index file >>= \case
    Left err -> pure (Just (file, err))
    Right loaded -> replaceInBuffer lns loaded

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine {line}) =
  - line

withReplaceEnv ::
  Members [Stop ReplaceError, Rpc !! RpcError, Rpc, Resource] r =>
  Sem r [Maybe (Path Abs File, RpcError)] ->
  Sem r ()
withReplaceEnv run = do
  withOption "hidden" True do
    errors <- run
    traverse_ (stop . ReplaceError.BufferErrors) (nonEmpty (catMaybes errors))

replaceLines ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  [(Text, GrepOutputLine)] ->
  Sem r ()
replaceLines lines' =
  withReplaceEnv do
    index <- buildBufferIndex
    traverse (replaceLinesInFile index) sorted
  where
    sorted = mapMaybe nonEmpty (Map.elems (sortOn lineNumberDesc <$> byFile lines'))

    byFile =
      flip foldr mempty \ a@(_, GrepOutputLine {file}) ->
        flip Map.alter file \ prev ->
          Just (a : fold prev)

replaceLinesFromBuffer ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  Buffer ->
  [(Text, GrepOutputLine)] ->
  Sem r ()
replaceLinesFromBuffer buf lines' = do
  bufferSetOption buf "buftype" ("nofile" :: Text)
  replaceLines lines'
  bufferSetOption buf "buftype" ("acwrite" :: Text)
  bufferSetOption buf "modified" False

deleteLines ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  [GrepOutputLine] ->
  Sem r ()
deleteLines lines' = replaceLines (zip (repeat "") lines')

replaceSave ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  Replace ->
  Sem r ()
replaceSave (Replace (ScratchState _ _ buffer _ _ _ _) lines') = do
  updatedLines <- bufferContent buffer
  if length updatedLines /= getSum (foldMap (Sum . length) lines')
  then badReplacement
  else replaceLinesFromBuffer buffer (zip updatedLines (join (Map.elems lines')))
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
