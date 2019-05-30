module Proteome.Init(
  initialize,
  proteomeStage2,
  proteomeStage4,
  proteomePoll,
  proteomeStage1,
  resolveMainProject,
  initWithMain,
  resolveAndInitMain,
) where

import qualified Control.Lens as Lens (set)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Neovim (Neovim, fromObject, vim_call_function')
import Neovim.Context.Internal (Config(customConfig), asks')
import Proteome.Config (logConfig, readConfig)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (_mainProject, mainProject)
import Proteome.Data.Project (
  Project(meta),
  ProjectMetadata(DirProject, VirtualProject),
  ProjectType(..),
  )
import Proteome.Data.Proteome (Proteome)
import qualified Proteome.Log as Log
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Project (pathData)
import Proteome.Project.Activate (activateProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import qualified Proteome.Settings as S
import Ribosome.Config.Setting (settingE, updateSetting)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)
import UnliftIO.Directory (getCurrentDirectory)
import UnliftIO.STM (TVar)

resolveMainProject :: Ribo e Project
resolveMainProject = do
  mainDir <- settingE S.mainProjectDir
  vimCwd <- vim_call_function' "getcwd" []
  cwd <- getCurrentDirectory
  (root, name, tpe) <- pathData (fromRight (fromRight cwd (fromObject vimCwd)) mainDir)
  resolveProjectFromConfig (Just root) name (Just tpe)

setMainProject :: Project -> Proteome ()
setMainProject project =
  Ribo.modify $ Lens.set Env._mainProject project

updateMainType :: Maybe ProjectType -> Proteome ()
updateMainType tpe = updateSetting S.mainType (fromMaybe (ProjectType "none") tpe)

setMainProjectVars :: ProjectMetadata -> Proteome ()
setMainProjectVars (DirProject name _ tpe) = do
  updateSetting S.mainName name
  updateMainType tpe
setMainProjectVars (VirtualProject name) = do
  updateSetting S.mainName name
  updateMainType (Just (ProjectType "virtual"))

initWithMain :: Project -> Proteome ()
initWithMain main = do
  Log.debugS $ "initializing with main project: " ++ show main
  setMainProject main
  setMainProjectVars (meta main)
  activateProject main

resolveAndInitMain :: Proteome ()
resolveAndInitMain = do
  main <- resolveMainProject
  initWithMain main

initialize' :: Proteome (Ribosome (TVar Env))
initialize' = do
  resolveAndInitMain
  asks' customConfig

initialize :: Neovim e (Ribosome (TVar Env))
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "proteome" def
  retypeNeovim (const ribo) initialize'

proteomeStage1 :: Proteome ()
proteomeStage1 = loadBuffers

proteomeStage2 :: Proteome ()
proteomeStage2 = do
  main <- Ribo.inspect Env.mainProject
  paths <- readConfig "project" main
  logConfig paths

proteomeStage4 :: Proteome ()
proteomeStage4 = do
  main <- Ribo.inspect Env.mainProject
  paths <- readConfig "project_after" main
  logConfig paths

proteomePoll :: Neovim e Bool
proteomePoll = return True
