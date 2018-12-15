{-# OPTIONS_GHC -F -pgmF htfpp #-}

module CycleSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import Neovim (Neovim)
import Test.Framework
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Unit (fixture)
import Proteome.Add (proAdd)
import Proteome.Project.Activate (proNext, proPrev)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Proteome (Proteome)
import Proteome.Init (resolveAndInitMain)
import qualified Proteome.Settings as S (projectBaseDirs, mainProjectDir)
import Proteome.Test.Unit (specWithDef)
import Config (vars)
import Project (cn, fn, tp, hask, prot, cil, flag)

assertProject :: FilePath -> String -> Neovim e ()
assertProject projectsDir n = do
  cwd <- nvimCwd
  liftIO $ assertEqual cwd $ projectsDir </> hask </> n

cycleSpec :: Proteome ()
cycleSpec = do
  projectsDir <- fixture "projects"
  let assertDir = assertProject projectsDir
  let mainDir = projectsDir </> hask </> prot
  updateSetting S.projectBaseDirs [projectsDir]
  updateSetting S.mainProjectDir mainDir
  resolveAndInitMain
  proAdd $ AddOptions fn tp
  proAdd $ AddOptions cn tp
  assertDir prot
  proNext
  assertDir flag
  proNext
  assertDir cil
  proNext
  assertDir prot
  proNext
  assertDir flag
  proPrev
  assertDir prot

test_next :: IO ()
test_next = vars >>= specWithDef cycleSpec
