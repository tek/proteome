{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddSpec(htf_thisModulesTests) where

import Path (parseAbsDir, parseRelDir, (</>))
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Unit (fixture)
import Test.Framework

import Config (vars)
import Project (flag, fn, hask, l, tp)
import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import qualified Proteome.Settings as Settings (projectConfig)
import Unit (specWithDef)

addSpec :: Proteome ()
addSpec = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  proAdd $ AddOptions fn tp (Just False)
  ps <- getL @Env Env.projects
  haskPath <- parseRelDir (toString hask)
  flagPath <- parseRelDir (toString flag)
  let root = projectsDir </> haskPath </> flagPath
  liftIO $ assertEqual [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] ps

test_add :: IO ()
test_add = vars >>= specWithDef addSpec
