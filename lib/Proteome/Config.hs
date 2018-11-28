{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Proteome.Config(
  proReadConfig,
  readConfig,
  ProjectConfig (..)
)
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map as Map
import Data.Map.Strict
import UnliftIO.STM (readTVar, atomically)
import Neovim
import Neovim.Log
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Project
import Proteome.Data.Proteome

newtype ProjectConfig =
  ProjectConfig {
    projectTypes :: Map ProjectType [FilePath]
  }
  deriving (Generic, NFData)

instance NvimObject ProjectConfig where
  toObject ProjectConfig {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("projectTypes", toObject projectTypes)
    ]

runtime :: FilePath -> Neovim a ()
runtime path = vim_command' $ "runtime " ++ path

runtimeConf :: FilePath -> String -> Neovim a ()
runtimeConf confDir path = runtime(confDir ++ "/" ++ path)

typeProjectConf :: FilePath -> ProjectName -> ProjectType -> Neovim a ()
typeProjectConf confDir (ProjectName name') (ProjectType tpe') = do
  runtimeConf confDir tpe'
  runtimeConf confDir $ tpe' ++ "/" ++ name'

readConfigMeta :: String -> Project -> Neovim a ()
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) = do
  void $ traverse (typeProjectConf confDir name') tpe'
readConfigMeta _ _ = return ()

readConfigProject :: String -> Project -> Neovim a ()
readConfigProject confDir project = do
  readConfigMeta confDir project
  void $ traverse (runtimeConf confDir) (fmap projectType (types project))

readConfig :: String -> Project -> Neovim a ()
readConfig confDir project = do
  runtimeConf confDir "all/*"
  readConfigProject confDir project

proReadConfig :: Proteome ()
proReadConfig = do
  liftIO $ debugM "ribo" "asdf"
  t <- ask
  main <- atomically $ fmap Env.mainProject $ readTVar t
  readConfig "project" main
