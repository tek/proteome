{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PersistStoreSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (set)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B (readFile)
import System.FilePath ((</>))
import Test.Framework
import Ribosome.Api.Buffer (edit)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Ribosome.Test.Unit (fixture, tempDir)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectMetadata(DirProject),
  _meta,
  )
import Proteome.PersistBuffers (storeBuffers, PersistBuffers(PersistBuffers))
import Proteome.Test.Unit (specWithDef)
import Config (vars)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot "") (Just (ProjectType "haskell"))

storeTarget :: FilePath -> FilePath -> FilePath -> PersistBuffers
storeTarget f1 f2 f3 = PersistBuffers (Just f2) [f1, f2, f3]

storeBuffersSpec :: Proteome ()
storeBuffersSpec = do
  let nonexistentFile = "nonexistent"
  base <- fixture "persist/store"
  let file1 = base </> "file1"
  let file2 = base </> "file2"
  let file3 = base </> "file3"
  edit file1
  edit file2
  edit file3
  edit $ base </> nonexistentFile
  edit file2
  persistDir <- tempDir "persist/store"
  Ribo.modify $ set (_mainProject._meta) main
  updateSetting persistenceDir persistDir
  storeBuffers
  let bufferFile = persistDir </> "proteome/haskell/flagellum/buffers.json"
  json <- liftIO $ B.readFile bufferFile
  pb <- case decode json of
    Just a -> return a
    _ -> fail "invalid json"
  liftIO $ assertEqual pb (storeTarget file1 file2 file3)

test_storeBuffers :: IO ()
test_storeBuffers =
  vars >>= specWithDef storeBuffersSpec
