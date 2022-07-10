module Proteome.Test.PersistStoreTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LByteString
import Path (Abs, File, Path, absdir, reldir, relfile, toFilePath, (</>))
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (edit)
import Ribosome.Persist (Persist, PersistPath, persistRoot)
import Ribosome.Test (resumeTestError)

import Proteome.BufEnter (updateBuffers)
import Proteome.Data.PersistBuffers (PersistBuffers (PersistBuffers))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.PersistBuffers (storeBuffers)
import Proteome.Test.Run (interpretPersistTest, proteomeTest)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot [absdir|/|]) (Just (ProjectType "haskell"))

storeTarget :: Path Abs File -> Path Abs File -> Path Abs File -> PersistBuffers
storeTarget f1 f2 f3 =
  PersistBuffers (Just f2) [f2, f3, f1]

test_storeBuffers :: UnitTest
test_storeBuffers =
  proteomeTest $ interpretPersistTest do
    let nonexistentFile = [relfile|nonexistent|]
    base <- Test.fixturePath [reldir|persist/store|]
    let file1 = base </> [relfile|file1|]
    let file2 = base </> [relfile|file2|]
    let file3 = base </> [relfile|file3|]
    e file1
    e file2
    e file3
    e $ base </> nonexistentFile
    e file2
    persistDir <- resumeTestError @PersistPath persistRoot
    atomicModify' (#mainProject . #meta .~ main)
    resumeTestError @(Persist _) storeBuffers
    let bufferFile = persistDir </> [relfile|buffers/haskell/flagellum/buffers.json|]
    js <- embed (LByteString.readFile (toFilePath bufferFile))
    pb <- case decode js of
      Just a -> pure a
      _ -> fail "invalid json"
    storeTarget file1 file2 file3 === pb
    where
      e a =
        edit a *> updateBuffers
