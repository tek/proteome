{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Proteome.Config(
  proReadConfig,
  readConfig,
  ProjectConfig (..),
)
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map as Map
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Text.Prettyprint.Doc ((<+>), viaShow)
import Neovim (NvimObject(..), Dictionary, Object(ObjectMap), vim_command')
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Internal.NvimObject (extractObject)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Project (
  Project(Project, types),
  ProjectType(ProjectType, projectType),
  ProjectLang,
  ProjectName(ProjectName),
  ProjectMetadata(DirProject),
  )
import Proteome.Data.Proteome

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

runtime :: FilePath -> Ribo a ()
runtime path = vim_command' $ "runtime " ++ path ++ ".vim"

runtimeConf :: FilePath -> String -> Ribo a ()
runtimeConf confDir path = runtime(confDir ++ "/" ++ path)

typeProjectConf :: FilePath -> ProjectName -> ProjectType -> Ribo a ()
typeProjectConf confDir (ProjectName name') (ProjectType tpe') = do
  runtimeConf confDir tpe'
  runtimeConf confDir $ tpe' ++ "/" ++ name'

readConfigMeta :: String -> Project -> Ribo a ()
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) =
  traverse_ (typeProjectConf confDir name') tpe'
readConfigMeta _ _ = return ()

readConfigProject :: String -> Project -> Ribo a ()
readConfigProject confDir project = do
  readConfigMeta confDir project
  traverse_ (runtimeConf confDir) (fmap projectType (types project))

readConfig :: String -> Project -> Ribo a ()
readConfig confDir project = do
  runtimeConf confDir "all/*"
  readConfigProject confDir project

proReadConfig :: Proteome ()
proReadConfig = do
  main <- Ribo.inspect Env.mainProject
  readConfig "project" main
  readConfig "project_after" main
