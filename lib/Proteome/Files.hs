module Proteome.Files where

import Control.Lens (view)
import qualified Data.Map as Map (fromList)
import Path (Abs, Dir, File, Path, parseAbsDir, toFilePath)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuContinue, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, markedMenuItems)
import Ribosome.Msgpack.Error (DecodeError)

import Proteome.Data.Env (Env)
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError(..))
import Proteome.Files.Process (files)
import Proteome.Files.Syntax (filesSyntax)

editFile ::
  NvimE e m =>
  Menu (Path Abs File) ->
  Prompt ->
  m (MenuConsumerAction m (), Menu (Path Abs File))
editFile menu _ =
  action menu
  where
    action =
      maybe menuContinue quit marked
    quit =
      menuQuitWith . traverse_ (edit . toFilePath)
    marked =
      view MenuItem.meta <$$> markedMenuItems menu

actions ::
  NvimE e m =>
  MonadRibo m =>
  [(Text, Menu (Path Abs File) -> Prompt -> m (MenuConsumerAction m (), Menu (Path Abs File)))]
actions =
  [
    ("cr", editFile)
    ]

filesWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  PromptConfig m ->
  Path Abs Dir ->
  [Path Abs Dir] ->
  m ()
filesWith promptConfig cwd paths =
  void $ nvimMenu scratchOptions (files cwd paths) handler promptConfig Nothing
  where
    scratchOptions =
      scratchSyntax [filesSyntax] . defaultScratchOptions $ "proteome-files"
    handler =
      defaultMenu (Map.fromList actions)

proFiles ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  [Path Abs Dir] ->
  m ()
proFiles paths = do
  cwd <- hoistEitherAs FilesError.BadCwd =<< parseAbsDir <$> nvimCwd
  filesWith (defaultPrompt False) cwd paths
