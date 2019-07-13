module Proteome.Files where

import Control.Lens (view)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (take)
import Path (Abs, Dir, File, Path, parseAbsDir, parseRelDir, toFilePath, (</>))
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.Setting (Setting(Setting))
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
import Text.RE.PCRE.Text (RE, compileRegex)

import Proteome.Data.Env (Env)
import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError(..))
import Proteome.Files.Process (files)
import Proteome.Files.Syntax (filesSyntax)
import qualified Proteome.Settings as Settings (filesExcludeDirectories, filesExcludeFiles, filesExcludeHidden)

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

parsePath :: Path Abs Dir -> Text -> Maybe (Path Abs Dir)
parsePath _ path | Text.take 1 path == "/" =
  parseAbsDir (toString path)
parsePath cwd path =
  (cwd </>) <$> parseRelDir (toString path)

readRE ::
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  Text ->
  Text ->
  m RE
readRE name text =
  tryHoistAnyAs (FilesError.BadRE name text) (compileRegex (toString text))

readREs ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  MonadDeepError e SettingError m =>
  Setting [Text] ->
  m [RE]
readREs s@(Setting name _ _) =
  traverse (readRE name) =<< setting s

filesConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  m FilesConfig
filesConfig =
  FilesConfig <$> hidden <*> fs <*> dirs
  where
    hidden =
      setting Settings.filesExcludeHidden
    fs =
      readREs Settings.filesExcludeFiles
    dirs =
      readREs Settings.filesExcludeDirectories

filesWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  PromptConfig m ->
  Path Abs Dir ->
  [Text] ->
  m ()
filesWith promptConfig cwd paths = do
  conf <- filesConfig
  void $ nvimMenu scratchOptions (files conf cwd nePaths) handler promptConfig Nothing
  where
    nePaths =
      fromMaybe (cwd :| []) $ nonEmpty absPaths
    absPaths =
      mapMaybe (parsePath cwd) paths
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
  [Text] ->
  m ()
proFiles paths = do
  cwd <- hoistEitherAs FilesError.BadCwd =<< parseAbsDir <$> nvimCwd
  filesWith (defaultPrompt True) cwd paths
