module Proteome.Buffers where

import Control.Lens (each, elemOf, uses)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  Handler,
  HandlerError,
  Rpc,
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  pathText,
  resumeHandlerError,
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
  MenuItem (MenuItem),
  MenuSem,
  MenuWidget,
  MenuWrite,
  PromptConfig,
  defaultPrompt,
  deleteSelected,
  interpretMenu,
  menuWrite,
  semState,
  staticNvimMenuDef,
  unselected,
  withFocus,
  withMappings,
  withSelection',
  )
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
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
  MenuWrite ListedBuffer r =>
  MenuWidget r BufferAction
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
  MenuSem ListedBuffer r (Maybe (MenuAction a))
deleteWith deleter =
  withSelection' \ delete -> do
    keep <- semState (uses unselected (fmap MenuItem.meta))
    compensateForMissingActiveBuffer delete keep
    deleteListedBuffersWith deleter delete
    deleteSelected
    pure MenuAction.Render

moveCurrentLast ::
  Member Rpc r =>
  [MenuItem ListedBuffer] ->
  Sem r [MenuItem ListedBuffer]
moveCurrentLast items = do
  current <- vimGetCurrentBuffer
  pure $ spin current items []
  where
    spin current (item : rest) result | lens item == current =
      result ++ rest ++ [item]
    spin current (item : rest) result =
      spin current rest (item : result)
    spin _ [] result =
      result
    lens =
      ListedBuffer.buffer . MenuItem.meta

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
  MenuWrite ListedBuffer r =>
  Mappings r BufferAction
actions =
  [
    ("cr", load),
    ("d", menuWrite (deleteWith "bdelete")),
    ("D", menuWrite (deleteWith "bdelete!")),
    ("w", menuWrite (deleteWith "bwipeout")),
    ("W", menuWrite (deleteWith "bwipeout!"))
  ]

bufferAction ::
  Member Rpc r =>
  BufferAction ->
  Sem r ()
bufferAction = \case
  Load buf ->
    loadListedBuffer buf

buffersWith ::
  Members [Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  Members [AtomicState Env, Settings !! SettingError, Rpc, Rpc !! RpcError, Scratch, Stop HandlerError] r =>
  PromptConfig ->
  Sem r ()
buffersWith promptConfig =
  interpretMenu $ withMappings actions do
    bufs <- buffers
    result <- staticNvimMenuDef scratchOptions bufs promptConfig
    handleResult "buffers" bufferAction result
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
  Members [AtomicState Env, Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError] r =>
  Members [ChronosTime, Log, Mask res, Race, Resource, Embed IO, Final IO] r =>
  Handler r ()
proBuffers =
  resumeHandlerError @Rpc $ resumeHandlerError @Scratch do
    buffersWith =<< defaultPrompt []
