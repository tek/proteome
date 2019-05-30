{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddSpec(htf_thisModulesTests) where

import Config (vars)
import Control.Monad.IO.Class (liftIO)
import Project (flag, fn, hask, l, tp)
import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (projects)
import Proteome.Data.Project (
  Project(Project),
  ProjectMetadata(DirProject),
  ProjectRoot(ProjectRoot),
  )
import Proteome.Data.Proteome (Proteome)
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Test.Unit (specWithDef)
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))
import Test.Framework

addSpec :: Proteome ()
addSpec = do
  projectsDir <- fixture "projects"
  updateSetting S.projectBaseDirs [projectsDir]
  proAdd $ AddOptions fn tp False
  ps <- Ribo.inspect projects
  let root = projectsDir </> hask </> flag
  liftIO $ assertEqual [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] ps

test_add :: IO ()
test_add = vars >>= specWithDef addSpec
