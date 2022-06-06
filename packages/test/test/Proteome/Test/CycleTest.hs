module Proteome.Test.CycleTest where

import Path (reldir, (</>))
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimCwd)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (testError)

import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions (AddOptions))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Init (resolveAndInitMain)
import Proteome.Project.Activate (proNext, proPrev)
import qualified Proteome.Settings as Settings (mainProjectDir, projectConfig)
import qualified Proteome.Test.Dirs as Dirs
import Proteome.Test.Project (cn, fn, tp)
import Proteome.Test.Run (proteomeTest)

test_cycle :: UnitTest
test_cycle =
  proteomeTest do
    projectsDir <- Test.fixturePath [reldir|projects|]
    let
      assertDir d =
        withFrozenCallStack do
          cwd <- nvimCwd
          projectsDir </> Dirs.hask </> d === cwd
    let mainDir = projectsDir </> Dirs.hask </> Dirs.prot
    Settings.update Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
    Settings.update Settings.mainProjectDir mainDir
    testError resolveAndInitMain
    proAdd (AddOptions fn tp (Just False))
    proAdd (AddOptions cn tp (Just False))
    assertDir Dirs.prot
    let checkNext n = proNext *> assertDir n
    traverse_ @[] checkNext [Dirs.flag, Dirs.cil, Dirs.prot, Dirs.flag]
    proPrev
    assertDir Dirs.prot
