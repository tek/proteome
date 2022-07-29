module Proteome.Init where

import Exon (exon)
import qualified Log
import Ribosome (Handler, Rpc, RpcError, SettingError, Settings, resumeHandlerError)
import Ribosome.Api (nvimCallFunction, uautocmd)
import qualified Ribosome.Settings as Settings

import Proteome.Config (logConfig, readConfig)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject, VirtualProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Project.Activate (activateProject)
import Proteome.Project.Resolve (fromRootSettings)
import qualified Proteome.Settings as Settings (mainName, mainProjectDir, mainType)

resolveMainProject ::
  Members [Settings, Settings !! SettingError, Rpc, Stop ResolveError, Embed IO] r =>
  Sem r Project
resolveMainProject = do
  mainDir <- Settings.maybe Settings.mainProjectDir
  vimCwd <- nvimCallFunction "getcwd" []
  fromRootSettings (ProjectRoot (fromMaybe vimCwd mainDir))

updateMainType ::
  Member Settings r =>
  Maybe ProjectType ->
  Sem r ()
updateMainType tpe =
  Settings.update Settings.mainType (fromMaybe (ProjectType "none") tpe)

setMainProjectVars ::
  Member Settings r =>
  ProjectMetadata ->
  Sem r ()
setMainProjectVars = \case
  DirProject name _ tpe -> do
    Settings.update Settings.mainName name
    updateMainType tpe
  VirtualProject name -> do
    Settings.update Settings.mainName name
    updateMainType (Just (ProjectType "virtual"))

initWithMain ::
  Members [AtomicState Env, Settings, Rpc, Log, Embed IO] r =>
  Project ->
  Sem r ()
initWithMain main@(Project meta _ _ _) = do
  Log.debug [exon|initializing with main project: #{show main}|]
  atomicModify' (#mainProject .~ main)
  setMainProjectVars meta
  activateProject main

resolveAndInitMain ::
  Members [AtomicState Env, Settings !! SettingError, Settings, Rpc, Stop ResolveError, Log, Embed IO] r =>
  Sem r ()
resolveAndInitMain =
  initWithMain =<< resolveMainProject

loadConfig ::
  Members [AtomicState Env, Rpc] r =>
  Text ->
  Sem r ()
loadConfig dir =
  logConfig =<< readConfig dir =<< atomicGets Env.mainProject

projectConfig ::
  Members [AtomicState Env, Rpc] r =>
  Sem r ()
projectConfig = do
  loadConfig "project"
  uautocmd "ProteomeProject"

projectConfigAfter ::
  Members [AtomicState Env, Rpc] r =>
  Sem r ()
projectConfigAfter = do
  loadConfig "project_after"
  uautocmd "ProteomeProjectAfter"
  uautocmd "RibosomeUpdateVariables"

proLoad ::
  Members [AtomicState Env, Rpc !! RpcError] r =>
  Handler r ()
proLoad =
  resumeHandlerError @Rpc projectConfig

proLoadAfter ::
  Members [AtomicState Env, Rpc !! RpcError] r =>
  Handler r ()
proLoadAfter =
  resumeHandlerError @Rpc projectConfigAfter
