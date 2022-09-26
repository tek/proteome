module Proteome.Test.InitTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimCommand, vimGetVar)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (testError)

import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Init (projectConfig, resolveAndInitMain)
import qualified Proteome.Settings as Settings
import Proteome.Test.Run (proteomeTest)

test_init :: UnitTest
test_init =
  proteomeTest do
    nvimCommand "autocmd User ProteomeProject let g:success = 13"
    testError resolveAndInitMain
    projectConfig
    tpe <- Settings.get Settings.mainType
    name <- Settings.get Settings.mainName
    ProjectName "flagellum" === name
    ProjectType "haskell" === tpe
    ((13 :: Int) ===) =<< vimGetVar "success"
