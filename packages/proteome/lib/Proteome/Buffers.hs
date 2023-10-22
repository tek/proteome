module Proteome.Buffers where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Exon (exon)
import Lens.Micro.Extras (view)
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
  bufferIsFile,
  buflisted,
  ensureMainWindow,
  nvimBufIsLoaded,
  nvimCommand,
  nvimCwd,
  setCurrentBuffer,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  )
import Ribosome.Menu (
  MenuAction (Render),
  MenuApp,
  MenuItem (MenuItem),
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  RenderAnchor (AnchorLine),
  deleteSelected,
  fuzzy,
  menuState,
  modal,
  staticWindowMenu,
  unselected,
  use,
  withFocus,
  withSelection',
  )
import Ribosome.Menu.App (withInsert)
import Ribosome.Scratch (ScratchOptions (..))
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

type BuffersState =
  ModalState ListedBuffer

loadListedBuffer ::
  Member Rpc r =>
  ListedBuffer ->
  Sem r ()
loadListedBuffer (ListedBuffer buffer number _) =
  ifM (nvimBufIsLoaded buffer) (setCurrentBuffer buffer) (nvimCommand [exon|buffer #{show number}|])

load ::
  MenuWidget BuffersState r BufferAction
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
  when (any (\ b -> current == b ^. #buffer) marked) (loadListedBuffer next)
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
      unwords (show . (.number) <$> NonEmpty.toList bufs)

deleteWith ::
  Member Rpc r =>
  Text ->
  MenuWidget BuffersState r a
deleteWith deleter =
  menuState $ withSelection' \ delete -> do
    keep <- fmap (view #meta) <$> use unselected
    compensateForMissingActiveBuffer delete keep
    deleteListedBuffersWith deleter delete
    deleteSelected
    pure (Just (Render AnchorLine))

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
  bufs <- filterM bufferIsFile =<< filterM buflisted =<< atomicGets (.buffers)
  numbers <- traverse bufferGetNumber bufs
  names <- traverse bufferGetName bufs
  let items = item (pathText cwd) (padding numbers) <$> zip3 bufs numbers names
  ifM (Settings.or False Settings.buffersCurrentLast) (moveCurrentLast items) (pure items)
  where
    padding = Text.length . show . fromMaybe 0 . maximum
    item cwd pad (buf, num, name) =
      MenuItem (ListedBuffer buf num name) text [text]
      where
        text = " * " <> padded pad (show num) <> "  " <> strip cwd name
    padded pad num = Text.replicate (pad - Text.length num) " " <> num
    strip cwd name = fromMaybe name $ Text.stripPrefix cwd name

actions ::
  Member Rpc r =>
  MenuApp BuffersState r BufferAction
actions =
  [
    (withInsert "<cr>", load),
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

type BuffersStack =
  [
    ModalWindowMenus ListedBuffer !! RpcError,
    AtomicState Env,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log
  ]

buffersMenu ::
  ∀ r .
  Members BuffersStack r =>
  Members [Rpc, Stop Report] r =>
  Sem r ()
buffersMenu = do
  items <- buffers
  result <- mapReport do
    staticWindowMenu items (modal fuzzy) (def & #items .~ scratchOptions) actions
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
  Members BuffersStack r =>
  Handler r ()
proBuffers =
  resumeReport @Rpc do
    buffersMenu
