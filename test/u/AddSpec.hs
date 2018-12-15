{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import Test.Framework
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Ribosome.Test.Unit (fixture)
import Proteome.Data.Proteome (Proteome)
import Proteome.Add (proAdd)
import Proteome.Data.Env (projects)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Project (
  ProjectRoot(ProjectRoot),
  ProjectMetadata(DirProject),
  Project(Project),
  )
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Test.Unit (specWithDef)
import Config (vars)
import Project (flag, hask, fn, tp, l)

addSpec :: Proteome ()
addSpec = do
  projectsDir <- fixture "projects"
  updateSetting S.projectBaseDirs [projectsDir]
  proAdd $ AddOptions fn tp
  ps <- Ribo.inspect projects
  let root = projectsDir </> hask </> flag
  liftIO $ assertEqual [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] ps

test_add :: IO ()
test_add = vars >>= specWithDef addSpec
