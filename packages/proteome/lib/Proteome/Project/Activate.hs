module Proteome.Project.Activate where

import Control.Lens ((%~), (.~))
import Exon (exon)
import Path.IO (doesDirExist)
import Ribosome (Handler, Rpc, RpcError, SettingError, Settings, pathText, resumeHandlerError)
import Ribosome.Api (echo, nvimCommand)
import Ribosome.Data.PluginName (PluginName)
import qualified Ribosome.Settings as Settings

import Proteome.Data.ActiveProject (ActiveProject (ActiveProject))
import Proteome.Data.Env (Env)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Project (allProjects, currentProject)
import qualified Proteome.Settings as Settings

activeProject :: Project -> ActiveProject
activeProject (Project (DirProject name _ tpe) _ lang _) = ActiveProject name (fromMaybe (ProjectType "none") tpe) lang
activeProject (Project (VirtualProject name) _ lang _) = ActiveProject name (ProjectType "virtual") lang

activateDirProject ::
  Members [Rpc, Embed IO] r =>
  ProjectMetadata ->
  Sem r ()
activateDirProject (DirProject _ (ProjectRoot root) _) = do
  whenM (doesDirExist root) do
    nvimCommand [exon|chdir #{pathText root}|]
activateDirProject _ =
  unit

activateProject ::
  Members [Settings, Rpc, Embed IO] r =>
  Project ->
  Sem r ()
activateProject project@(Project meta _ _ _) = do
  Settings.update Settings.active (activeProject project)
  activateDirProject meta

describeProject :: ProjectMetadata -> Text
describeProject (DirProject (ProjectName name) _ (Just (ProjectType tpe))) = tpe <> "/" <> name
describeProject (DirProject (ProjectName name) _ Nothing) = name
describeProject (VirtualProject (ProjectName name)) = name

echoProjectActivation ::
  Members [Reader PluginName, Rpc] r =>
  Project ->
  Sem r ()
echoProjectActivation (Project meta _ _ _) =
  echo [exon|activated project #{describeProject meta}|]

activateCurrentProject ::
  Members [Settings, AtomicState Env, Reader PluginName, Rpc, Embed IO] r =>
  Sem r ()
activateCurrentProject = do
  pro <- currentProject
  mapM_ activateProject pro
  mapM_ echoProjectActivation pro

setProjectIndex ::
  Member (AtomicState Env) r =>
  Int ->
  Sem r ()
setProjectIndex index = do
  pros <- allProjects
  for_ (index `mod` length pros) \ i ->
    atomicModify' (#currentProjectIndex .~ i)

cycleProjectIndex ::
  Member (AtomicState Env) r =>
  (Int -> Int) ->
  Sem r ()
cycleProjectIndex f = do
  pros <- allProjects
  atomicModify' $ #currentProjectIndex %~ \ i -> fromMaybe i (f i `rem` length pros)

selectProject ::
  Members [Settings, AtomicState Env, Reader PluginName, Rpc, Embed IO] r =>
  Int ->
  Sem r ()
selectProject index = do
  setProjectIndex index
  activateCurrentProject

proPrev ::
  Members [Settings !! SettingError, AtomicState Env, Reader PluginName, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
proPrev =
  resumeHandlerError @Rpc $ resumeHandlerError @Settings do
    cycleProjectIndex (subtract 1)
    activateCurrentProject

proNext ::
  Members [Settings !! SettingError, AtomicState Env, Reader PluginName, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
proNext =
  resumeHandlerError @Rpc $ resumeHandlerError @Settings do
    cycleProjectIndex (+1)
    activateCurrentProject
