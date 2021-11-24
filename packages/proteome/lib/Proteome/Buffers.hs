module Proteome.Buffers where

import Control.Lens (each, elemOf, uses, view)
import Control.Monad.Catch (MonadCatch)
import Data.Foldable (maximum)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import Ribosome.Api.Buffer (bufferIsFile, buflisted, setCurrentBuffer)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Window (ensureMainWindow)
import Ribosome.Config.Setting (settingOr)
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import qualified Ribosome.Menu.Consumer as Consumer
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuConsumer (MenuWidget, MenuWidgetM)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.MenuState (menuWrite)
import Ribosome.Menu.ItemLens (unselected)
import Ribosome.Menu.Items (deleteSelected, withSelection')
import Ribosome.Menu.Items.Read (withFocusM)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (staticNvimMenu)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (
  bufferGetName,
  bufferGetNumber,
  nvimBufIsLoaded,
  vimCommand,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  )

import Proteome.Buffers.Syntax (buffersSyntax)
import qualified Proteome.Data.Env as Env
import Proteome.Data.Env (Env)
import qualified Proteome.Data.ListedBuffer as ListedBuffer
import Proteome.Data.ListedBuffer (ListedBuffer (ListedBuffer))
import qualified Proteome.Settings as Settings

loadListedBuffer ::
  NvimE e m =>
  ListedBuffer ->
  m ()
loadListedBuffer (ListedBuffer buffer number _) =
  ifM (nvimBufIsLoaded buffer) (setCurrentBuffer buffer) (vimCommand [exon|buffer #{show number}|])

load ::
  MonadIO m =>
  NvimE e m =>
  MonadBaseControl IO m =>
  MenuWidget m ListedBuffer ()
load =
  withFocusM loadListedBuffer

compensateForMissingActiveBuffer ::
  NvimE e m =>
  NonEmpty ListedBuffer ->
  [ListedBuffer] ->
  m ()
compensateForMissingActiveBuffer _ [] =
  vimCommand "enew"
compensateForMissingActiveBuffer marked (next : _) = do
  prev <- vimGetCurrentWindow
  void ensureMainWindow
  current <- vimGetCurrentBuffer
  when (elemOf (each . ListedBuffer.buffer) current marked) (loadListedBuffer next)
  vimSetCurrentWindow prev

deleteListedBuffersWith ::
  NvimE e m =>
  Text ->
  NonEmpty ListedBuffer ->
  m ()
deleteListedBuffersWith deleter bufs =
  vimCommand [exon|#{deleter} #{numbers}|]
  where
    numbers =
      unwords (show . view ListedBuffer.number <$> NonEmpty.toList bufs)

deleteWith ::
  NvimE e m =>
  Text ->
  MenuWidgetM m ListedBuffer ()
deleteWith deleter =
  withSelection' \ delete -> do
    keep <- uses unselected (fmap  MenuItem._meta)
    compensateForMissingActiveBuffer delete keep
    deleteListedBuffersWith deleter delete
    deleteSelected
    pure MenuAction.Render

moveCurrentLast ::
  NvimE e m =>
  [MenuItem ListedBuffer] ->
  m [MenuItem ListedBuffer]
moveCurrentLast items = do
  current <- vimGetCurrentBuffer
  return $ spin current items []
  where
    spin current (item : rest) result | view lens item == current =
      result ++ rest ++ [item]
    spin current (item : rest) result =
      spin current rest (item : result)
    spin _ [] result =
      result
    lens =
      MenuItem.meta . ListedBuffer.buffer

buffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  m [MenuItem ListedBuffer]
buffers = do
  cwd <- (<> "/") <$> nvimCwd
  bufs <- filterM bufferIsFile =<< filterM buflisted =<< getL @Env Env.buffers
  numbers <- traverse bufferGetNumber bufs
  names <- traverse bufferGetName bufs
  let items = item (toText cwd) (padding numbers) <$> zip3 bufs numbers names
  ifM (settingOr False Settings.buffersCurrentLast) (moveCurrentLast items) (return items)
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
  MonadIO m =>
  NvimE e m =>
  MonadBaseControl IO m =>
  [(Text, MenuWidget m ListedBuffer ())]
actions =
  [
    ("cr", load),
    ("d", menuWrite (deleteWith "bdelete")),
    ("D", menuWrite (deleteWith "bdelete!")),
    ("w", menuWrite (deleteWith "bwipeout")),
    ("W", menuWrite (deleteWith "bwipeout!"))
  ]

buffersWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  PromptConfig m ->
  m ()
buffersWith promptConfig = do
  bufs <- buffers
  void $ staticNvimMenu scratchOptions bufs handler promptConfig
  where
    scratchOptions =
      def {
        _name = name,
        _syntax = [buffersSyntax],
        _filetype = Just name
      }
    name =
      "proteome-buffers"
    handler =
      Consumer.withMappings (Map.fromList actions)

proBuffers ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  m ()
proBuffers =
  buffersWith (defaultPrompt [])
