module Proteome.Add where

import Control.Lens (view)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text
import Neovim (CommandArguments (CommandArguments))
import Path (Abs, Dir, Path, dirname, parent, stripProperPrefix)
import Path.IO (listDir)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Items (traverseSelection_)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptFlag (StartInsert))
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Streamly.Prelude as Stream

import Proteome.Add.Syntax (addSyntax)
import Proteome.Data.AddError (AddError)
import qualified Proteome.Data.AddError as AddError (AddError (InvalidProjectSpec))
import Proteome.Data.AddItem (AddItem (AddItem))
import Proteome.Data.AddOptions (AddOptions (AddOptions))
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project (Project))
import qualified Proteome.Data.ProjectConfig as ProjectConfig
import Proteome.Data.ProjectConfig (ProjectConfig)
import Proteome.Data.ProjectMetadata (ProjectMetadata (VirtualProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Path (dropSlash, pathText)
import Proteome.Project.Activate (selectProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import qualified Proteome.Settings as Settings

add ::
  ∀ s e m .
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  Maybe ProjectType ->
  Bool ->
  m ()
add name tpe activate = do
  addDirProject =<< resolveProjectFromConfig Nothing name tpe
  when activate (selectProject (-1))
  where
    addDirProject (Project (VirtualProject _) _ _ _) =
      pure ()
    addDirProject project =
      modifyL @Env Env.projects \ p -> p ++ [project]

proAdd ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  AddOptions ->
  m ()
proAdd (AddOptions name tpe activate) =
  add name (Just tpe) (fromMaybe False activate)

addFromName ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  Bool ->
  m ()
addFromName name =
  add name Nothing

proAddCmd ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e AddError m =>
  MonadDeepError e ResolveError m =>
  CommandArguments ->
  Text ->
  m ()
proAddCmd (CommandArguments bang _ _ _) spec =
  process (Text.splitOn "/" spec)
  where
    process [tpe, name] =
      add (ProjectName name) (Just (ProjectType tpe)) activate
    process [name] =
      addFromName (ProjectName name) activate
    process _ =
      throwHoist (AddError.InvalidProjectSpec spec)
    activate =
      fromMaybe False bang

availableProjectsInBase ::
  MonadIO m =>
  Path Abs Dir ->
  m [MenuItem AddItem]
availableProjectsInBase base =
  fmap (fmap cons . join) . traverse list =<< list base
  where
    list =
      fmap fst . listDir
    cons proj =
      MenuItem (AddItem tpe name) pt (fromMaybe pt (pathText <$> stripProperPrefix proj base))
      where
        tpe =
          dropSlash (dirname (parent proj))
        name =
          dropSlash (dirname proj)
        pt =
          pathText proj

availableProjects ::
  MonadIO m =>
  ProjectConfig ->
  m [MenuItem AddItem]
availableProjects (view ProjectConfig.baseDirs -> dirs) =
  join <$> traverse availableProjectsInBase dirs

menuAdd ::
  ∀ s e m .
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MenuWidget m AddItem ()
menuAdd =
  traverseSelection_ \ (AddItem tpe name) ->
    lift (add (ProjectName name) (Just (ProjectType tpe)) True)

actions ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadBaseControl IO m =>
  [(Text, MenuWidget m AddItem ())]
actions =
  [
    ("cr", menuAdd)
    ]

addMenuWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadDeepError e DecodeError m =>
  PromptConfig m ->
  m ()
addMenuWith promptConfig = do
  projectConfig <- setting Settings.projectConfig
  void $ nvimMenu scratchOptions (mList (availableProjects projectConfig)) handler promptConfig
  where
    mList =
      Stream.fromList <=< Stream.fromEffect
    scratchOptions =
      scratchSyntax [addSyntax] . defaultScratchOptions $ "proteome-add"
    handler =
      Consumer.withMappings (Map.fromList actions)

proAddMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ResolveError m =>
  MonadDeepError e DecodeError m =>
  m ()
proAddMenu =
  addMenuWith (defaultPrompt [StartInsert])
