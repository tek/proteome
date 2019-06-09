{-# OPTIONS_GHC -F -pgmF htfpp #-}

module CycleSpec (htf_thisModulesTests) where

import Control.Monad.Catch (MonadThrow)
import Path (Abs, Dir, Path, parseAbsDir, parseRelDir, (</>))
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Unit (fixture)
import Test.Framework

import Config (vars)
import Project (cil, cn, flag, fn, hask, prot, tp)
import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Proteome)
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Init (resolveAndInitMain)
import Proteome.Project.Activate (proNext, proPrev)
import qualified Proteome.Settings as Settings (mainProjectDir, projectConfig)
import Unit (specWithDef)

assertProject ::
  AssertM m =>
  NvimE e m =>
  MonadThrow m =>
  Path Abs Dir ->
  Text ->
  m ()
assertProject projectsDir n = do
  cwd <- parseAbsDir =<< nvimCwd
  haskPath <- parseRelDir (toString hask)
  nPath <- parseRelDir (toString n)
  gassertEqual (projectsDir </> haskPath </> nPath) cwd

cycleSpec :: Proteome ()
cycleSpec = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  let assertDir = assertProject projectsDir
  haskPath <- parseRelDir (toString hask)
  protPath <- parseRelDir (toString prot)
  let mainDir = projectsDir </> haskPath </> protPath
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  updateSetting Settings.mainProjectDir mainDir
  resolveAndInitMain
  proAdd $ AddOptions fn tp (Just False)
  proAdd $ AddOptions cn tp (Just False)
  assertDir prot
  let checkNext n = proNext *> assertDir n
  traverse_ checkNext [flag, cil, prot, flag]
  proPrev
  assertDir prot

test_cycle :: IO ()
test_cycle = vars >>= specWithDef cycleSpec
