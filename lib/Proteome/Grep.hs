module Proteome.Grep where

import Conduit ((.|))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (null)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Register (setregLine)
import Ribosome.Api.Window (setCurrentCursor)
import Ribosome.Config.Setting (setting)
import qualified Ribosome.Data.Register as Register (Register(Special))
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSize, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuContinue, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError(EmptyPattern))
import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import Proteome.Grep.Line (uniqueGrepLines)
import Proteome.Grep.Process (grep, grepCmdline)
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
  MonadRibo m =>
  Menu GrepOutputLine ->
  Prompt ->
  m (MenuConsumerAction m (), Menu GrepOutputLine)
selectResult menu _ =
  check $ selectedMenuItem menu
  where
    check (Just (MenuItem (GrepOutputLine path line col _) _ _)) =
      menuQuitWith (navigate path line col) menu
    check Nothing =
      menuContinue menu

yankResult ::
  NvimE e m =>
  MonadRibo m =>
  Menu GrepOutputLine ->
  Prompt ->
  m (MenuConsumerAction m (), Menu GrepOutputLine)
yankResult menu _ =
  check $ selectedMenuItem menu
  where
    check (Just (MenuItem (GrepOutputLine _ _ _ text) _ _)) =
      menuQuitWith (setregLine (Register.Special "\"") [text]) menu
    check Nothing =
      menuContinue menu

proGrepWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  PromptConfig m ->
  Text ->
  Text ->
  m ()
proGrepWith promptConfig path patt = do
  grepper <- setting Settings.grepCmdline
  cwd <- toText <$> nvimCwd
  (exe, args) <- grepCmdline grepper patt cwd path
  void $ nvimMenu scratchOptions (grep cwd exe args .| uniqueGrepLines) handler promptConfig Nothing
  where
    scratchOptions =
      scratchSize 1 . scratchSyntax [grepSyntax] . defaultScratchOptions $ "proteome-grep"
    handler =
      defaultMenu (Map.fromList [("cr", selectResult), ("y", yankResult)])

proGrepIn ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Text ->
  Text ->
  m ()
proGrepIn =
  proGrepWith (defaultPrompt [])

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
  MonadThrow m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Maybe Text ->
  m ()
proGrep patt = do
  cwd <- nvimCwd
  nonemptyPattern <- maybe askPattern pure patt
  proGrepIn (toText cwd) nonemptyPattern
