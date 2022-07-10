module Proteome.Config where

import Exon (exon)
import Ribosome (Handler, Rpc, RpcError, msgpackArray, resumeHandlerError)
import Ribosome.Api (nvimGetOption, vimCallFunction, vimCommand)

import qualified Proteome.Data.Env as Env
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Project as Project
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectType (ProjectType (ProjectType))

globRtp ::
  Member Rpc r =>
  Text ->
  Sem r [Text]
globRtp path = do
  rtp :: Text <- nvimGetOption "runtimepath"
  vimCallFunction "globpath" (msgpackArray rtp path False True)

runtime ::
  Member Rpc r =>
  Text ->
  Sem r [Text]
runtime path = do
  vimCommand [exon|runtime! #{fpath}|]
  globRtp fpath
  where
    fpath = path <> ".vim"

runtimeConf ::
  Member Rpc r =>
  Text ->
  Text ->
  Sem r [Text]
runtimeConf confDir path =
  runtime [exon|#{confDir}/#{path}|]

typeProjectConf ::
  Member Rpc r =>
  Text ->
  ProjectName ->
  ProjectType ->
  Sem r [Text]
typeProjectConf confDir (ProjectName name') (ProjectType tpe') = do
  tpePaths <- runtimeConf confDir tpe'
  namePaths <- runtimeConf confDir $ tpe' <> "/" <> name'
  pure $ tpePaths <> namePaths

readConfigMeta ::
  Member Rpc r =>
  Text ->
  Project ->
  Sem r [Text]
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) = do
  paths <- traverse (typeProjectConf confDir name') tpe'
  pure $ fromMaybe [] paths
readConfigMeta _ _ = pure []

readConfigProject ::
  Member Rpc r =>
  Text ->
  Project ->
  Sem r [Text]
readConfigProject confDir project = do
  paths <- traverse (runtimeConf confDir . coerce) (Project.types project)
  metaPaths <- readConfigMeta confDir project
  pure $ join paths <> metaPaths

readConfig ::
  Member Rpc r =>
  Text ->
  Project ->
  Sem r [Text]
readConfig confDir project = do
  allPaths <- runtimeConf confDir "all/*"
  projectPaths <- readConfigProject confDir project
  pure $ allPaths <> projectPaths

logConfig ::
  Member (AtomicState Env) r =>
  [Text] ->
  Sem r ()
logConfig paths =
  atomicModify' (#configLog %~ (paths <>))

proReadConfig ::
  Members [Rpc !! RpcError, AtomicState Env] r =>
  Handler r ()
proReadConfig = do
  resumeHandlerError @Rpc do
    main <- atomicGets Env.mainProject
    configs <- readConfig "project" main
    logConfig configs
    afterConfigs <- readConfig "project_after" main
    logConfig afterConfigs

defaultTypeMarkers :: Map ProjectType [Text]
defaultTypeMarkers =
  [
    (ProjectType "haskell", ["stack.yaml", "*.cabal", "cabal.project"]),
    (ProjectType "scala", ["*.sbt"])
  ]
