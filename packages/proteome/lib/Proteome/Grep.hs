module Proteome.Grep where

import Control.Lens (view)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Register (setregLine)
import Ribosome.Api.Window (setCurrentCursor)
import Ribosome.Config.Setting (setting)
import qualified Ribosome.Data.Register as Register (Register (Special))
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Items (withFocusM, withSelectionM)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (IsStream, SerialT)

import Proteome.Data.Env (Env)
import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError (EmptyPattern))
import qualified Proteome.Data.GrepOutputLine as GrepOutputLine
import Proteome.Data.GrepOutputLine (GrepOutputLine (GrepOutputLine))
import Proteome.Data.ReplaceError (ReplaceError)
import Proteome.Grep.Process (grepCmdline, grepMenuItems)
import Proteome.Grep.Replace (deleteLines, replaceBuffer)
import Proteome.Grep.Syntax (grepSyntax)
import qualified Proteome.Settings as Settings (grepCmdline)

navigate ::
  NvimE e m =>
  Text ->
  Int ->
  Maybe Int ->
  m ()
navigate path line col = do
  edit (toString path)
  setCurrentCursor line (fromMaybe 0 col)
  vimCommand "normal! zv"
  vimCommand "normal! zz"

selectResult ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuWidget m GrepOutputLine ()
selectResult = do
  withFocusM \ (GrepOutputLine path line col _) ->
    navigate path line col

yankResult ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuWidget m GrepOutputLine ()
yankResult =
  withFocusM \ (GrepOutputLine _ _ _ txt) ->
    setregLine (Register.Special "\"") [txt]

replaceResult ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MenuWidget m GrepOutputLine ()
replaceResult =
  withSelectionM replaceBuffer

deleteResult ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  MenuWidget m GrepOutputLine ()
deleteResult =
  withSelectionM (deleteLines . toList)

menuItemSameLine :: MenuItem GrepOutputLine -> MenuItem GrepOutputLine -> Bool
menuItemSameLine (MenuItem l _ _) (MenuItem r _ _) =
  GrepOutputLine.sameLine l r

uniqBy ::
  Monad m =>
  IsStream t =>
  (a -> a -> Bool) ->
  t m a ->
  t m a
uniqBy f =
  Streamly.catMaybes .
  Streamly.smapM (flip check) (pure Nothing)
  where
    check new =
      pure . \case
        Just old | f old new ->
          (Just old, Nothing)
        Just _ ->
          (Just new, Just new)
        Nothing ->
          (Just new, Just new)

uniqueGrepLines ::
  Monad m =>
  IsStream t =>
  t m (MenuItem GrepOutputLine) ->
  t m (MenuItem GrepOutputLine)
uniqueGrepLines =
  uniqBy menuItemSameLine

grepItems ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e SettingError m =>
  Text ->
  Text ->
  [Text] ->
  m (SerialT m (MenuItem GrepOutputLine))
grepItems path patt opt = do
  grepper <- setting Settings.grepCmdline
  cwd <- toText <$> nvimCwd
  (exe, args) <- grepCmdline grepper patt cwd path opt
  pure (uniqueGrepLines (grepMenuItems cwd exe args))

actions ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e ReplaceError m =>
  [(Text, MenuWidget m GrepOutputLine ())]
actions =
  [
    ("cr", selectResult),
    ("y", yankResult),
    ("r", replaceResult),
    ("d", deleteResult)
  ]

proGrepWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ReplaceError m =>
  PromptConfig m ->
  Text ->
  Text ->
  [Text] ->
  m ()
proGrepWith promptConfig path patt opt = do
  items <- grepItems path patt opt
  void $ nvimMenu scratchOptions items handler promptConfig
  where
    scratchOptions =
      def {
        _name = name,
        _syntax = [grepSyntax],
        _size = Just 1,
        _filetype = Just name
      }
    name =
      "proteome-grep"
    handler =
      Consumer.withMappings (Map.fromList actions)

proGrepIn ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ReplaceError m =>
  Text ->
  Text ->
  m ()
proGrepIn path patt =
  proGrepWith (defaultPrompt []) path patt []

proGrepOpt ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ReplaceError m =>
  Text ->
  Text ->
  m ()
proGrepOpt opt patt = do
  cwd <- nvimCwd
  proGrepWith (defaultPrompt []) (toText cwd) patt (Text.words opt)

proGrepOptIn ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ReplaceError m =>
  Text ->
  Text ->
  Text ->
  m ()
proGrepOptIn path opt patt =
  proGrepWith (defaultPrompt []) path patt (Text.words opt)

askPattern ::
  NvimE e m =>
  MonadDeepError e GrepError m =>
  m Text
askPattern = do
  input <- vimCallFunction "input" [toMsgpack ("pattern: " :: Text)]
  if Text.null input then throwHoist GrepError.EmptyPattern else pure input

proGrep ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ReplaceError m =>
  Maybe Text ->
  m ()
proGrep patt = do
  nonemptyPattern <- maybe askPattern pure patt
  proGrepOpt "" nonemptyPattern

proGrepList ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e SettingError m =>
  Text ->
  Text ->
  Text ->
  m [GrepOutputLine]
proGrepList path opt patt = do
  items <- grepItems path patt (Text.words opt)
  fmap (view MenuItem.meta) <$> Streamly.toList items
