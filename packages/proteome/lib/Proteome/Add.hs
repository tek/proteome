module Proteome.Add where

import Control.Lens ((<>~))
import qualified Data.Text as Text
import Path (Abs, Dir, Path, dirname, parent, stripProperPrefix)
import Path.IO (listDir)
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, RpcError, Scratch, SettingError, Settings, mapHandlerError, pathText, resumeHandlerError)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Errors (pluginHandlerErrors)
import Ribosome.Host.Data.Bang (Bang (Bang))
import Ribosome.Menu (
  MenuItem (..),
  MenuWidget,
  MenuWrite,
  PromptConfig,
  PromptFlag (StartInsert),
  defaultPrompt,
  interpretMenu,
  staticNvimMenuDef,
  traverseSelection_,
  withMappings,
  )
import Ribosome.Scratch (syntax)
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
  resumeHandlerError @Settings $ resumeHandlerError @Rpc $ mapHandlerError do
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
  resumeHandlerError @Settings $ resumeHandlerError @Rpc $ mapHandlerError @ResolveError $ mapHandlerError @AddError do
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
      MenuItem (AddItem tpe name) pt (maybe pt pathText (stripProperPrefix proj base))
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
  MenuWrite AddItem r =>
  Members [Settings, Rpc, AtomicState Env, Reader PluginName, Stop ResolveError, Log, Resource, Embed IO] r =>
  MenuWidget r ()
menuAdd =
  traverseSelection_ \ (AddItem tpe name) ->
    add (ProjectName name) (Just (ProjectType tpe)) True

addMenuWith ::
  Members [Rpc !! RpcError, Rpc, Settings !! SettingError, Settings, Scratch, AtomicState Env, Reader PluginName] r =>
  Members [Stop ResolveError, Mask res, Stop AddError, Log, Resource, Race, Embed IO, Final IO] r =>
  PromptConfig ->
  Sem r ()
addMenuWith promptConfig =
  interpretMenu $ withMappings [("cr", menuAdd)] do
    projectConfig <- Settings.get Settings.projectConfig
    projects <- sort <$> availableProjects projectConfig
    void $ staticNvimMenuDef scratchOptions projects promptConfig
  where
    scratchOptions =
      (defaultScratchOptions "proteome-add") {
        syntax = [addSyntax]
      }

proAddMenu ::
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, AtomicState Env, Reader PluginName] r =>
  Members [ChronosTime, Mask res, Log, Resource, Race, Embed IO, Final IO] r =>
  Handler r ()
proAddMenu =
  pluginHandlerErrors $ mapHandlerError @AddError $ mapHandlerError @ResolveError do
    addMenuWith =<< defaultPrompt [StartInsert]
