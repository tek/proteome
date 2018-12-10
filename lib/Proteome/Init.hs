module Proteome.Init(
  initialize,
  proteomeStage2,
  proteomeStage4,
) where

import Control.Monad.Reader
import Data.Default.Class (Default(def))
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath (takeFileName, takeDirectory)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import Control.Monad.IO.Class (MonadIO)
import Neovim (Neovim)
import UnliftIO.STM (TVar, newTVarIO)
import Ribosome.Config.Setting (setting, settingE, updateSetting)
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Ribosome.File (canonicalPaths)
import Ribosome.Internal.IO (retypeNeovim)
import Proteome.Data.Env (Env(Env))
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Proteome
import Proteome.Data.Project (
  Project(meta),
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectMetadata(DirProject),
  )
import Proteome.Project.Resolve (resolveProject)
import Proteome.Config (readConfig)
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Log
import qualified Proteome.Settings as S

pathData :: MonadIO m => Either String FilePath -> m (ProjectRoot, ProjectName, ProjectType)
pathData override = do
  cwd <- liftIO $ either (const getCurrentDirectory) pure override
  absMainDir <- liftIO $ makeAbsolute cwd
  return (
    ProjectRoot absMainDir,
    ProjectName $ takeFileName absMainDir,
    ProjectType $ (takeFileName . takeDirectory) absMainDir
    )

mainProject :: Ribo e Project
mainProject = do
  mainDir <- settingE S.mainProjectDir
  (root, name, tpe) <- pathData mainDir
  baseDirs <- (canonicalPaths <=< setting) S.projectBaseDirs
  -- typeDirs <- setting S.projectTypeDirs
  explicit <- setting S.projects
  config <- setting S.projectConfig
  return $ resolveProject baseDirs explicit config root name (Just tpe)

loadPersistedBuffers :: Project -> Neovim e ()
loadPersistedBuffers _ = return ()

updateMainType :: Maybe ProjectType -> Ribo e ()
updateMainType (Just tpe) = updateSetting S.mainType tpe
updateMainType Nothing = return ()

setProjectVars :: ProjectMetadata -> Ribo e ()
setProjectVars (DirProject name _ tpe) = do
  updateSetting S.mainName name
  updateMainType tpe
setProjectVars _ = return ()

initWithMain :: Project -> Ribo e Env
initWithMain main = do
  debugS $ "initializing with main project: " ++ show main
  loadPersistedBuffers main
  setProjectVars (meta main)
  return $ Env main [] def

initialize' :: Ribo e Env
initialize' = do
  main <- mainProject
  initWithMain main

initialize :: Neovim e (TVar Env)
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  env <- retypeNeovim (Ribosome "proteome") initialize'
  newTVarIO env

proteomeStage1 :: Proteome ()
proteomeStage1 = loadBuffers

proteomeStage2 :: Proteome ()
proteomeStage2 = do
  main <- Ribo.inspect Env.mainProject
  readConfig "project" main

proteomeStage4 :: Proteome ()
proteomeStage4 = do
  main <- Ribo.inspect Env.mainProject
  readConfig "project_after" main
