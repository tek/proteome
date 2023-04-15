module Proteome.Test.AddTest where

import Path (parseRelDir, reldir, (</>))
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, (===))
import qualified Ribosome.Settings as Settings

import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions (AddOptions))
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import qualified Proteome.Settings as Settings (projectConfig)
import Proteome.Test.Project (flag, fn, hask, l, tp)
import Proteome.Test.Run (proteomeTest)

test_add :: UnitTest
test_add =
  proteomeTest do
    projectsDir <- Test.fixturePath [reldir|projects|]
    Settings.update Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
    proAdd $ AddOptions fn tp (Just False)
    ps <- atomicGets (.projects)
    haskPath <- stopNote "parse dir" (parseRelDir (toString hask))
    flagPath <-  stopNote "parse dir" (parseRelDir (toString flag))
    let root = projectsDir </> haskPath </> flagPath
    [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] === ps
