module Proteome.Config(
  proReadConfig,
  readConfig
)
where

import UnliftIO.STM (readTVar, atomically)
import Neovim
import Proteome.Data.Env
import Proteome.Data.Project
import Proteome.Data.Proteome

runtime :: String -> Neovim a ()
runtime path = vim_command' $ "runtime " ++ path

runtimeConf :: String -> String -> Neovim a ()
runtimeConf confDir path = runtime(confDir ++ "/" ++ path)

typeProjectConf :: String -> String -> String -> Neovim a ()
typeProjectConf confDir tpe' name' = do
  runtimeConf confDir tpe'
  runtimeConf confDir $ tpe' ++ "/" ++ name'

readConfigMeta :: String -> Project -> Neovim a ()
readConfigMeta confDir (Project (DirProject name' _ tpe') _ _ _) = do
  _ <- traverse (typeProjectConf confDir name') tpe'
  return ()
readConfigMeta _ _ = return ()

readConfigProject :: String -> Project -> Neovim a ()
readConfigProject confDir project = do
  readConfigMeta confDir project
  _ <- traverse (runtimeConf confDir) (types project)
  return ()

readConfig :: String -> (Maybe Project) -> Neovim a ()
readConfig confDir project = do
  runtimeConf confDir "all/*"
  _ <- traverse (readConfigProject confDir) project
  return ()

proReadConfig :: Proteome ()
proReadConfig = do
  t <- ask
  main' <- atomically $ do
    env <- readTVar t
    return $ main env
  readConfig "project" main'
