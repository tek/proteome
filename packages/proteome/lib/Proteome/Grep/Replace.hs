module Proteome.Grep.Replace where

import Control.Monad (foldM)
import Data.List (insertBy)
import qualified Data.Map.Strict as Map
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
  nvimBufSetExtmark,
  nvimCallFunction,
  nvimCommand,
  nvimCreateNamespace,
  nvimGetOption,
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

replaceLinesInFile ::
  Member (Rpc !! RpcError) r =>
  NonEmpty (Text, GrepOutputLine) ->
  Sem r (Maybe (Path Abs File, Text), Maybe Buffer)
replaceLinesInFile lns@((_, GrepOutputLine {file}) :| _) =
  either loadError withBuffer =<< runStop ensureBuffer
  where
    withBuffer (exists, buffer) =
      resuming writeError $ Ribosome.noautocmd do
        traverse_ (uncurry (replaceLine buffer)) lns
        pure (Nothing, transientBuffer)
      where
        writeError err = pure (Just (file, show err), transientBuffer)
        transientBuffer = justIf (not exists) buffer

    loadError err = pure (Just (file, err), Nothing)

    ensureBuffer =
      resumeHoist @RpcError show $ Ribosome.noautocmd do
        exists <- isJust <$> bufferForFile file
        if | exists -> nvimCommand [exon|silent checktime #{pathText file}|]
           | otherwise -> addBuffer (pathText file)
        nvimCommand [exon|silent call bufload('#{pathText file}')|]
        FileBuffer buffer _ <- stopNote "Buffer vanished after loading" =<< bufferForFile file
        pure (exists, buffer)

lineNumberDesc :: (Text, GrepOutputLine) -> Int
lineNumberDesc (_, GrepOutputLine {line}) =
  - line

withReplaceEnv ::
  Members [Stop ReplaceError, Rpc !! RpcError, Rpc, Resource] r =>
  Sem r [(Maybe (Path Abs File, Text), Maybe Buffer)] ->
  Sem r ()
withReplaceEnv run = do
  withOption "hidden" True $ Ribosome.noautocmd do
    (errors, transient) <- unzip <$> run
    resume_ (nvimCommand "wall")
    traverse_ (resume_ . wipeBuffer) (catMaybes transient)
    traverse_ (stop . ReplaceError.BufferErrors) (nonEmpty (catMaybes errors))

replaceLines ::
  Members [Rpc !! RpcError, Rpc, Resource, Stop ReplaceError] r =>
  [(Text, GrepOutputLine)] ->
  Sem r ()
replaceLines lines' =
  withReplaceEnv do
    traverse replaceLinesInFile sorted
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
