{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PersistLoadSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (set)
import System.FilePath ((</>))
import Test.Framework
import Neovim (vim_get_buffers', vim_get_current_buffer', buffer_get_name')
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Ribosome.Test.Unit (fixture)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectMetadata(DirProject),
  _meta,
  )
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Test.Unit (specWithDef)
import Config (vars)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot "") (Just (ProjectType "haskell"))

loadBuffersSpec :: Proteome ()
loadBuffersSpec = do
  Ribo.modify $ set (_mainProject._meta) main
  persistDir <- fixture "persist/load"
  fixDir <- fixture "persist/store"
  updateSetting persistenceDir persistDir
  loadBuffers
  buffers <- vim_get_buffers'
  liftIO $ assertEqual 3 (length buffers)
  active <- buffer_get_name' =<< vim_get_current_buffer'
  liftIO $ assertEqual active (fixDir </> "file2")

test_loadBuffers :: IO ()
test_loadBuffers =
  vars >>= specWithDef loadBuffersSpec
