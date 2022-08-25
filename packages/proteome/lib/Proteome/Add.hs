module Proteome.Add where

import qualified Data.Text as Text
import Path (Abs, Dir, Path, dirname, parent, stripProperPrefix)
import Path.IO (listDir)
import Ribosome (
  Bang (Bang),
  Handler,
  PluginName,
  Rpc,
  RpcError,
  SettingError,
  Settings,
  mapReport,
  pathText,
  resumeReport,
  )
import Ribosome.Menu (
  MenuItem (..),
  MenuLoops,
  MenuResult,
  MenuWidget,
  NvimMenuUi,
  WindowMenu,
  staticNvimMenu,
  traverseSelection_,
  )
import Ribosome.Scratch (scratch, syntax)
import qualified Ribosome.Settings as Settings

import Proteome.Add.Syntax (addSyntax)
import qualified Proteome.Data.AddError as AddError
import Proteome.Data.AddError (AddError)
import Proteome.Data.AddItem (AddItem (AddItem))
import Proteome.Data.AddOptions (AddOptions (AddOptions))
import Proteome.Data.Env (Env)
import Proteome.Data.Project (Project (Project))
import qualified Proteome.Data.ProjectConfig as ProjectConfig
import Proteome.Data.ProjectConfig (ProjectConfig)
import Proteome.Data.ProjectMetadata (ProjectMetadata (VirtualProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Path (dropSlash)
import Proteome.Project.Activate (selectProject)
import Proteome.Project.Resolve (fromNameSettings)
import qualified Proteome.Settings as Settings

add ::
  Members [Settings, Rpc, AtomicState Env, Reader PluginName, Stop ResolveError, Log, Embed IO] r =>
  ProjectName ->
  Maybe ProjectType ->
  Bool ->
  Sem r ()
add name tpe activate = do
  addDirProject =<< fromNameSettings name tpe
  when activate (selectProject (-1))
  where
    addDirProject (Project (VirtualProject _) _ _ _) =
      unit
    addDirProject project =
      atomicModify' (#projects <>~ [project])

proAdd ::
  Members [Settings !! SettingError, Rpc !! RpcError, AtomicState Env, Reader PluginName, Log, Embed IO] r =>
  AddOptions ->
  Handler r ()
proAdd (AddOptions name tpe activate) =
  resumeReport @Settings $ resumeReport @Rpc $ mapReport do
    add name (Just tpe) (fromMaybe False activate)

addFromName ::
  Members [Settings, Rpc, AtomicState Env, Reader PluginName, Stop ResolveError, Log, Embed IO] r =>
  ProjectName ->
  Bool ->
  Sem r ()
addFromName name =
  add name Nothing

proAddCmd ::
  Members [Settings !! SettingError, Rpc !! RpcError, AtomicState Env, Reader PluginName, Log, Embed IO] r =>
  Bang ->
  Text ->
  Handler r ()
proAddCmd bang spec =
  resumeReport @Settings $ resumeReport @Rpc $ mapReport @ResolveError $ mapReport @AddError do
    process (Text.splitOn "/" spec)
  where
    process [tpe, name] =
      add (ProjectName name) (Just (ProjectType tpe)) activate
    process [name] =
      addFromName (ProjectName name) (bang == Bang)
    process _ =
      stop (AddError.InvalidProjectSpec spec)
    activate =
      bang == Bang

availableProjectsInBase ::
  Members [Stop AddError, Embed IO] r =>
  Path Abs Dir ->
  Sem r [MenuItem AddItem]
availableProjectsInBase base =
  fmap (fmap cons . join) . traverse list =<< list base
  where
    list d =
      stopEitherWith AddError.Directory =<< tryAny (fst <$> listDir d)
    cons proj =
      MenuItem (AddItem tpe name) pt (maybe pt dropSlash (stripProperPrefix base proj))
      where
        tpe =
          dropSlash (dirname (parent proj))
        name =
          dropSlash (dirname proj)
        pt =
          pathText proj

availableProjects ::
  Members [Stop AddError, Embed IO] r =>
  ProjectConfig ->
  Sem r [MenuItem AddItem]
availableProjects (ProjectConfig.baseDirs -> dirs) =
  join <$> traverse availableProjectsInBase dirs

menuAdd ::
  Members [Settings, Rpc, AtomicState Env, Reader PluginName, Stop ResolveError, Log, Embed IO] r =>
  MenuWidget AddItem r ()
menuAdd =
  traverseSelection_ \ (AddItem tpe name) ->
    add (ProjectName name) (Just (ProjectType tpe)) True

type AddStack ui =
  [
    NvimMenuUi ui,
    MenuLoops AddItem,
    AtomicState Env,
    Reader PluginName,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log,
    Embed IO
  ]

addMenu ::
  Members (AddStack ui) r =>
  Members [Rpc, Settings, Stop ResolveError, Stop AddError, Stop RpcError] r =>
  Sem r (MenuResult ())
addMenu = do
  projectConfig <- Settings.get Settings.projectConfig
  projects <- sort <$> availableProjects projectConfig
  staticNvimMenu projects def scratchOptions [("<cr>", menuAdd)]
  where
    scratchOptions =
      (scratch "proteome-add") { syntax = [addSyntax] }

proAddMenu ::
  Members (AddStack WindowMenu) r =>
  Handler r ()
proAddMenu =
  resumeReport @Rpc $
  resumeReport @Settings $
  mapReport @AddError $
  mapReport @ResolveError $
  mapReport @RpcError do
    void $ addMenu
