module Proteome.Buffers where

import Control.Lens (elemOf, view)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Exon (exon)
import Ribosome (
  Handler,
  Report,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  mapReport,
  pathText,
  resumeReport,
  )
import Ribosome.Api (
  bufferGetName,
  bufferGetNumber,
  nvimBufIsLoaded,
  nvimCommand,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  )
import Ribosome.Api.Buffer (bufferIsFile, buflisted, setCurrentBuffer)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Menu (
  Mappings,
  MenuAction (Render),
  MenuItem (MenuItem),
  MenuLoops,
  MenuWidget,
  NvimMenuUi,
  WindowMenu,
  deleteSelected,
  menuState,
  staticNvimMenu,
  unselected,
  use,
  withFocus,
  withSelection',
  )
import qualified Ribosome.Settings as Settings

import Proteome.Buffers.Syntax (buffersSyntax)
import qualified Proteome.Data.Env as Env
import Proteome.Data.Env (Env)
import qualified Proteome.Data.ListedBuffer as ListedBuffer
import Proteome.Data.ListedBuffer (ListedBuffer (ListedBuffer))
import Proteome.Menu (handleResult)
import qualified Proteome.Settings as Settings

newtype BufferAction =
  Load ListedBuffer
  deriving stock (Eq, Show)

loadListedBuffer ::
  Member Rpc r =>
  ListedBuffer ->
  Sem r ()
loadListedBuffer (ListedBuffer buffer number _) =
  ifM (nvimBufIsLoaded buffer) (setCurrentBuffer buffer) (nvimCommand [exon|buffer #{show number}|])

load ::
  MenuWidget ListedBuffer r BufferAction
load =
  withFocus (pure . Load)

compensateForMissingActiveBuffer ::
  Member Rpc r =>
  NonEmpty ListedBuffer ->
  [ListedBuffer] ->
  Sem r ()
compensateForMissingActiveBuffer _ [] =
  nvimCommand "enew"
compensateForMissingActiveBuffer marked (next : _) = do
  prev <- vimGetCurrentWindow
  void ensureMainWindow
  current <- vimGetCurrentBuffer
  when (elemOf (each . #buffer) current marked) (loadListedBuffer next)
  vimSetCurrentWindow prev

deleteListedBuffersWith ::
  Member Rpc r =>
  Text ->
  NonEmpty ListedBuffer ->
  Sem r ()
deleteListedBuffersWith deleter bufs =
  nvimCommand [exon|#{deleter} #{numbers}|]
  where
    numbers =
      unwords (show . ListedBuffer.number <$> NonEmpty.toList bufs)

deleteWith ::
  Member Rpc r =>
  Text ->
  MenuWidget ListedBuffer r a
deleteWith deleter =
  menuState $ withSelection' \ delete -> do
    keep <- fmap (view #meta) <$> use unselected
    compensateForMissingActiveBuffer delete keep
    deleteListedBuffersWith deleter delete
    deleteSelected
    pure Render

moveCurrentLast ::
  Member Rpc r =>
  [MenuItem ListedBuffer] ->
  Sem r [MenuItem ListedBuffer]
moveCurrentLast items = do
  current <- vimGetCurrentBuffer
  pure $ spin current items []
  where
    spin current (item : rest) result | item ^. lens == current =
      result ++ rest ++ [item]
    spin current (item : rest) result =
      spin current rest (item : result)
    spin _ [] result =
      result
    lens =
      #meta . #buffer

buffers ::
  Members [AtomicState Env, Settings !! SettingError, Rpc, Rpc !! RpcError] r =>
  Sem r [MenuItem ListedBuffer]
buffers = do
  cwd <- nvimCwd
  bufs <- filterM bufferIsFile =<< filterM buflisted =<< atomicGets Env.buffers
  numbers <- traverse bufferGetNumber bufs
  names <- traverse bufferGetName bufs
  let items = item (pathText cwd) (padding numbers) <$> zip3 bufs numbers names
  ifM (Settings.or False Settings.buffersCurrentLast) (moveCurrentLast items) (pure items)
  where
    padding =
      Text.length . show . maximum
    item cwd pad (buf, num, name) =
      MenuItem (ListedBuffer buf num name) text' text'
      where
        text' =
          " * " <> padded pad (show num) <> "  " <> strip cwd name
    padded pad num =
      Text.replicate (pad - Text.length num) " " <> num
    strip cwd name =
      fromMaybe name $ Text.stripPrefix cwd name

actions ::
  Member Rpc r =>
  Mappings ListedBuffer r BufferAction
actions =
  [
    ("<cr>", load),
    ("d", deleteWith "bdelete"),
    ("D", deleteWith "bdelete!"),
    ("w", deleteWith "bwipeout"),
    ("W", deleteWith "bwipeout!")
  ]

bufferAction ::
  Member Rpc r =>
  BufferAction ->
  Sem r ()
bufferAction = \case
  Load buf ->
    loadListedBuffer buf

type BuffersStack ui =
  [
    NvimMenuUi ui,
    MenuLoops ListedBuffer,
    AtomicState Env,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log
  ]

buffersMenu ::
  ∀ ui r .
  Members (BuffersStack ui) r =>
  Members [Rpc, Stop Report] r =>
  Sem r ()
buffersMenu = do
  items <- buffers
  result <- mapReport do
    staticNvimMenu items def scratchOptions actions
  handleResult bufferAction result
  where
    scratchOptions =
      def {
        name = ScratchId name,
        syntax = [buffersSyntax],
        filetype = Just name
      }
    name =
      "proteome-buffers"

proBuffers ::
  Members (BuffersStack WindowMenu) r =>
  Handler r ()
proBuffers =
  resumeReport @Rpc do
    buffersMenu
