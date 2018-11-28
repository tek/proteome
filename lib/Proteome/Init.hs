module Proteome.Init(
  initialize,
  proteomeStage2,
  proteomeStage4,
) where

import Control.Monad.Reader
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName, takeDirectory)
import Control.Monad.IO.Class (MonadIO)
import Neovim.Context.Internal (Neovim)
import UnliftIO.STM (TVar, newTVarIO)
import Ribosome.Config.Settings (setting, settingE, updateSetting)
import Ribosome.Data.Ribo
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Ribosome.Internal.IO (retypeNeovim)
import Proteome.Data.Env (Env(Env))
import Proteome.Data.Proteome
import Proteome.Data.Project (Project(meta), ProjectName(..), ProjectType(..), ProjectMetadata(DirProject))
import Proteome.Project.Resolve (resolveProject)
import qualified Proteome.Settings as S

pathData :: MonadIO m => Either String FilePath -> m (FilePath, ProjectName, ProjectType)
pathData override = do
  cwd <- liftIO $ either (const getCurrentDirectory) pure override
  return (cwd, ProjectName $ takeFileName cwd, ProjectType $ (takeFileName . takeDirectory) cwd)

mainProject :: Ribo e Project
mainProject = do
  mainDir <- settingE S.mainProjectDir
  (root, name, tpe) <- pathData mainDir
  baseDirs <- setting S.projectBaseDirs
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
  loadPersistedBuffers main
  setProjectVars (meta main)
  return $ Env main []

initialize' :: Ribo e Env
initialize' = do
  main <- mainProject
  initWithMain main

initialize :: Neovim e (TVar Env)
initialize = do
  env <- retypeNeovim (Ribosome "proteome") initialize'
  newTVarIO env

proteomeStage2 :: Proteome ()
proteomeStage2 = return ()

proteomeStage4 :: Proteome ()
proteomeStage4 = return ()
