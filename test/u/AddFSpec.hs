{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddFSpec(htf_thisModulesTests) where

import Config (vars)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map (fromList)
import Neovim (toObject, vim_call_function', vim_command')
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Test.Functional (fixture, specWith)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Control.Ribo (Ribo)
import System.FilePath ((</>))
import Test.Framework

addSpec :: Ribo env ()
addSpec = do
  projectsDir <- fixture "projects"
  updateSetting S.projectBaseDirs [projectsDir]
  _ <- vim_call_function' "ProAddProject" [toObject $ Map.fromList [
    ("name" :: String, toObject ("cilia" :: String)),
    ("tpe", toObject ("haskell" :: String)),
    ("activate", toObject True)
    ]]
  cwd <- nvimCwd
  liftIO $ assertEqual (projectsDir </> "haskell" </> "cilia") cwd

test_addFunction :: IO ()
test_addFunction =
  vars >>= specWith addSpec

addCommandSpec :: Ribo env ()
addCommandSpec = do
  projectsDir <- fixture "projects"
  updateSetting S.projectBaseDirs [projectsDir]
  _ <- vim_command' "ProAdd! haskell/cilia"
  cwd <- nvimCwd
  liftIO $ assertEqual (projectsDir </> "haskell" </> "cilia") cwd

test_addCommand :: IO ()
test_addCommand =
  vars >>= specWith addCommandSpec
