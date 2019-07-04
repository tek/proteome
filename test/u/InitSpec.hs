{-# OPTIONS_GHC -F -pgmF htfpp #-}

module InitSpec (htf_thisModulesTests) where

import Config (vars)
import qualified Proteome.Settings as Settings
import Ribosome.Config.Setting (setting)
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Init (proteomeStage2, resolveAndInitMain)
import Unit (specWithDef)

initSpec :: Proteome ()
initSpec = do
  vimCommand "autocmd User ProteomeProject let g:success = 13"
  resolveAndInitMain
  proteomeStage2
  tpe <- setting Settings.mainType
  name <- setting Settings.mainName
  gassertEqual (ProjectName "flagellum") name
  gassertEqual (ProjectType "haskell") tpe
  gassertEqual (13 :: Int) =<< vimGetVar "success"

test_init :: IO ()
test_init =
  vars >>= specWithDef initSpec
