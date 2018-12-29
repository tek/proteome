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

import Data.Default.Class (Default(def))
import qualified Control.Lens as Lens (set)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import Control.Monad.IO.Class (liftIO)
import UnliftIO.Directory (getCurrentDirectory)
import UnliftIO.STM (TVar)
import Neovim (Neovim, vim_call_function', fromObject)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Config.Setting (settingE, updateSetting)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Internal.IO (retypeNeovim)
import qualified Proteome.Data.Env as Env (mainProject, _mainProject)
import Proteome.Data.Env (Env)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project(meta),
  ProjectType(..),
  ProjectMetadata(DirProject, VirtualProject),
  )
import Proteome.Project (pathData)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import Proteome.Project.Activate (activateProject)
import Proteome.Config (readConfig, logConfig)
import Proteome.PersistBuffers (loadBuffers)
import qualified Proteome.Log as Log
import qualified Proteome.Settings as S

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
