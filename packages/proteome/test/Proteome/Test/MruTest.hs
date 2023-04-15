module Proteome.Test.MruTest where

import Path (Abs, File, Path, relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest, assertJust)
import Ribosome (Rpc)
import Ribosome.Api (bufferPath, edit, vimCommand)

import Proteome.BufEnter (updateBuffers)
import qualified Proteome.Data.Env as Env
import Proteome.Test.Run (proteomeTest)

setupBuffers ::
  Members [Test, Rpc] r =>
  Sem r (Path Abs File, Path Abs File, Path Abs File)
setupBuffers = do
  buf1 <- Test.fixturePath [relfile|buffers/buf1|]
  buf2 <- Test.fixturePath [relfile|buffers/buf2|]
  buf3 <- Test.fixturePath [relfile|buffers/buf3|]
  edit buf1
  edit buf2
  edit buf3
  pure (buf1, buf2, buf3)

test_mru :: UnitTest
test_mru =
  proteomeTest do
    (buf1, buf2, buf3) <- setupBuffers
    edit buf3
    updateBuffers
    edit buf2
    updateBuffers
    edit buf1
    updateBuffers
    vimCommand "bdelete! 2"
    updateBuffers
    assertJust [buf1, buf3] . sequence =<< traverse bufferPath =<< atomicGets (.buffers)
