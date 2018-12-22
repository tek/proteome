{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Proteome.Config(
  proReadConfig,
  readConfig,
  ProjectConfig (..),
  logConfig,
)
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Lens (over)
import Control.Monad (join)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc ((<+>), viaShow)
import Neovim (NvimObject(..), Dictionary, Object(ObjectMap), vim_command')
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Ribosome.Api.Function (callFunction)
import Ribosome.Api.Option (optionString)
import Ribosome.Internal.NvimObject (extractObject)
import Proteome.Data.Env (_configLog)
import Proteome.Data.Project (
  Project(Project, types),
  ProjectType(ProjectType, projectType),
  ProjectLang,
  ProjectName(ProjectName),
  ProjectMetadata(DirProject),
  )
import Proteome.Data.Proteome
import Proteome.Env (getMainProject)

data ProjectConfig =
  ProjectConfig {
    projectTypes :: Map ProjectType [FilePath],
    typeMap :: Map ProjectType [ProjectType],
    langMap :: Map ProjectType [ProjectLang]
  }
  deriving (Generic, NFData)

instance NvimObject ProjectConfig where
  toObject ProjectConfig {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("projectTypes", toObject projectTypes),
      ("typeMap", toObject typeMap)
    ]
  fromObject (ObjectMap o) = do
    projectTypes' <- extractObject "projectTypes" o
    typeMap' <- extractObject "typeMap" o
    langMap' <- extractObject "langMap" o
    return $ ProjectConfig projectTypes' typeMap' langMap'
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
