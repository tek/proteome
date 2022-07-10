module Proteome.Test.AddMenuTest where

import Path (reldir, (</>))
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, (===))
import Ribosome.Menu (interpretNvimMenuFinal, promptInput)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (testError)

import Proteome.Add (addMenu)
import Proteome.Data.AddError (AddError)
import qualified Proteome.Data.Env as Env
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ResolveError (ResolveError)
import qualified Proteome.Settings as Settings
import qualified Proteome.Test.Dirs as Dirs
import Proteome.Test.Project (fn, l, tp)
import Proteome.Test.Run (proteomeTest)

addChars :: [Text]
addChars =
  ["k", "cr"]

test_addMenu :: UnitTest
test_addMenu =
  proteomeTest do
    projectsDir <- Test.fixturePath [reldir|projects|]
    Settings.update Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
    testError @AddError (testError @ResolveError do
      interpretNvimMenuFinal (promptInput addChars addMenu))
    projects <- atomicGets Env.projects
    let root = projectsDir </> Dirs.hask </> Dirs.flag
    [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] === projects
