{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Ribosome.Test.Unit (fixture)
import Proteome.Data.Proteome (Proteome)
import Proteome.Add (proAdd)
import Proteome.Data.Env (projects)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Project (ProjectName(ProjectName), ProjectType(ProjectType))
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Test.Unit (specWithDef)
import Config (vars)

addSpec :: Proteome ()
addSpec = do
  projectsDir <- fixture "projects"
  updateSetting S.projectBaseDirs [projectsDir]
  proAdd $ AddOptions (ProjectName "flagellum") (ProjectType "haskell")
  ps <- Ribo.inspect projects
  liftIO $ assertEqual 1 (length ps)

test_add :: IO ()
test_add = vars >>= specWithDef addSpec
