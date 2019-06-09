{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module PersistLoadSpec (htf_thisModulesTests) where

import Data.String.QM (qt)
import qualified Data.Text.IO as Text (writeFile)
import Path (absdir)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Nvim.Api.IO (bufferGetName, vimGetBuffers, vimGetCurrentBuffer, vimSetOption)
import Ribosome.Test.Unit (fixture, tempDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Framework

import Config (vars)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (mainProject)
import qualified Proteome.Data.Project as Project (meta)
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.PersistBuffers (loadBuffers)
import Unit (specWithDef)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot [absdir|/|]) (Just (ProjectType "haskell"))

buffersJson :: Text -> Text
buffersJson base =
  [qt|{"current":"${base}/file2","buffers":["${base}/file1","${base}/file2","${base}/file3"]}|]

loadBuffersSpec :: Proteome ()
loadBuffersSpec = do
  vimSetOption "swapfile" (toMsgpack False)
  setL @Env (Env.mainProject . Project.meta) main
  persistBase <- tempDir "persist/load"
  let persistDir = persistBase </> "proteome/haskell/flagellum"
  liftIO $ createDirectoryIfMissing True persistDir
  fixDir <- fixture "persist/store"
  liftIO $ Text.writeFile (persistDir </> "buffers.json") (buffersJson (toText fixDir))
  updateSetting persistenceDir persistBase
  loadBuffers
  buffers <- vimGetBuffers
  gassertEqual 3 (length buffers)
  active <- bufferGetName =<< vimGetCurrentBuffer
  gassertEqual (toString active) (fixDir </> "file2")

test_loadBuffers :: IO ()
test_loadBuffers =
  vars >>= specWithDef loadBuffersSpec
