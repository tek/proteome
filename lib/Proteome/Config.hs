{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Config
where

import Control.DeepSeq (NFData)
import Control.Lens (over)
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Map as Map (fromList)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc (viaShow, (<+>))
import GHC.Generics (Generic)
import Neovim (Dictionary, NvimObject(..), Object(ObjectMap), vim_command')
import Ribosome.Api.Function (callFunction)
import Ribosome.Api.Option (optionString)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (modify)
import Ribosome.Internal.NvimObject (extractObject)

import Proteome.Data.Env (_configLog)
import Proteome.Data.Project (
  Project(Project, types),
  ProjectLang,
  ProjectMetadata(DirProject),
  ProjectName(ProjectName),
  ProjectType(ProjectType, projectType),
  )
import Proteome.Data.Proteome
import Proteome.Env (getMainProject)

data ProjectConfig =
  ProjectConfig {
    projectTypes :: Map ProjectType [FilePath],
    typeMap :: Map ProjectType [ProjectType],
    typeMarkers :: Map ProjectType [FilePath],
    langMap :: Map ProjectType ProjectLang,
    langsMap :: Map ProjectLang [ProjectLang]
  }
  deriving (Generic, NFData)

instance NvimObject ProjectConfig where
  toObject ProjectConfig {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("projectTypes", toObject projectTypes),
      ("typeMap", toObject typeMap),
      ("typeMarkers", toObject typeMarkers),
      ("langMap", toObject langMap),
      ("langsMap", toObject langsMap)
    ]
  fromObject (ObjectMap o) = do
    projectTypes' <- extractObject "projectTypes" o
    typeMap' <- extractObject "typeMap" o
    typeMarkers' <- extractObject "typeMarkers" o
    langMap' <- extractObject "langMap" o
    langsMap' <- extractObject "langsMap" o
    return $ ProjectConfig projectTypes' typeMap' typeMarkers' langMap' langsMap'
  fromObject o = Left ("invalid type for ProjectConfig: " <+> viaShow o)

globRtp :: FilePath -> Ribo a [FilePath]
globRtp path = do
  rtp <- optionString "runtimepath"
  callFunction "globpath" [toObject rtp, toObject path, toObject False, toObject True]

runtime :: FilePath -> Ribo a [FilePath]
runtime path = do
  vim_command' $ "runtime! " ++ fpath
  globRtp fpath
  where fpath = path ++ ".vim"

runtimeConf :: FilePath -> String -> Ribo a [FilePath]
runtimeConf confDir path = runtime (confDir ++ "/" ++ path)

typeProjectConf :: FilePath -> ProjectName -> ProjectType -> Ribo a [FilePath]
typeProjectConf confDir (ProjectName name') (ProjectType tpe') = do
  tpePaths <- runtimeConf confDir tpe'
  namePaths <- runtimeConf confDir $ tpe' ++ "/" ++ name'
  return $ tpePaths ++ namePaths

readConfigMeta :: String -> Project -> Ribo a [FilePath]
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) = do
  paths <- traverse (typeProjectConf confDir name') tpe'
  return $ fromMaybe [] paths
readConfigMeta _ _ = return []

readConfigProject :: String -> Project -> Ribo a [FilePath]
readConfigProject confDir project = do
  paths <- traverse (runtimeConf confDir) (fmap projectType (types project))
  metaPaths <- readConfigMeta confDir project
  return $ join paths ++ metaPaths

readConfig :: String -> Project -> Ribo a [FilePath]
readConfig confDir project = do
  allPaths <- runtimeConf confDir "all/*"
  projectPaths <- readConfigProject confDir project
  return $ allPaths ++ projectPaths

logConfig :: [FilePath] -> Proteome ()
logConfig paths =
  Ribo.modify $ over _configLog (paths ++)

proReadConfig :: Proteome ()
proReadConfig = do
  main <- getMainProject
  configs <- readConfig "project" main
  logConfig configs
  afterConfigs <- readConfig "project_after" main
  logConfig afterConfigs

defaultTypeMarkers :: Map ProjectType [String]
defaultTypeMarkers =
  Map.fromList [
    (ProjectType "haskell", ["stack.yaml", "*.cabal"]),
    (ProjectType "scala", ["*.sbt"])
    ]
