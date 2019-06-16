{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module AddFSpec (htf_thisModulesTests) where

import qualified Data.Map as Map (fromList)
import Path (parseAbsDir, reldir, (</>))
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimSetVar)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Unit (fixture)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Plugin (plugin')

addSpecN :: Proteome () -> Proteome ()
addSpecN request = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  () <- vimSetVar "proteome_project_config" (toMsgpack $ ProjectConfig [projectsDir] def def def def def def)
  () <- request
  await (gassertEqual (projectsDir </> [reldir|haskell|] </> [reldir|cilia|])) (parseAbsDir =<< nvimCwd)

addSpec :: Proteome () -> IO ()
addSpec request = do
  ribo <- newRibosome "proteome" def
  integrationSpecDef (plugin' ribo) (addSpecN request)

test_addFunction :: IO ()
test_addFunction =
  addSpec request
  where
    request =
      vimCallFunction "ProAddProject" [toMsgpack $ Map.fromList [
        ("name" :: Text, toMsgpack ("cilia" :: Text)),
        ("tpe", toMsgpack ("haskell" :: Text)),
        ("activate", toMsgpack True)
        ]]

test_addCommand :: IO ()
test_addCommand =
  addSpec request
  where
    request =
      vimCommand "ProAdd! haskell/cilia"
