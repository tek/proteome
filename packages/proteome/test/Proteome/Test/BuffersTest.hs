module Proteome.Test.BuffersTest where

import Path (Abs, File, Path, relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest, assertEq, assertJust, unitTest)
import Ribosome (Rpc, pathText)
import Ribosome.Api (bufferPath, currentBufferPath, vimGetBuffers)
import Ribosome.Api.Buffer (bufferForFile, buflisted, edit)
import qualified Ribosome.Data.FileBuffer as FileBuffer
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Interpreter.Menu (promptInput)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent (Mapping))
import qualified Ribosome.Settings as Settings
import Test.Tasty (TestTree, testGroup)

import Proteome.Buffers (buffers, buffersMenu)
import Proteome.Data.Env (Env)
import qualified Proteome.Data.ListedBuffer as ListedBuffer (name)
import qualified Proteome.Settings as Settings (buffersCurrentLast)
import Proteome.Test.Run (proteomeTest)

setupBuffers ::
  Members [AtomicState Env, Rpc, Test] r =>
  Sem r (Path Abs File, Path Abs File, Path Abs File)
setupBuffers = do
  buf1 <- Test.fixturePath [relfile|buffers/buf1|]
  buf2 <- Test.fixturePath [relfile|buffers/buf2|]
  buf3 <- Test.fixturePath [relfile|buffers/buf3|]
  edit buf1
  edit buf2
  edit buf3
  bufs <- traverse (fmap (fmap FileBuffer.buffer) . bufferForFile) [buf3, buf2, buf1]
  atomicModify' (#buffers .~ catMaybes bufs)
  pure (buf1, buf2, buf3)

bufferLines :: [Text]
bufferLines =
  [
    " * 3  test/fixtures/buffers/buf3",
    " * 2  test/fixtures/buffers/buf2",
    " * 1  test/fixtures/buffers/buf1"
  ]

-- TODO check that paths are relative to nvimCwd
test_bufferPath :: UnitTest
test_bufferPath =
  proteomeTest do
    (_, _, _) <- setupBuffers
    assertEq bufferLines . fmap MenuItem.text =<< buffers

test_loadBuffer :: UnitTest
test_loadBuffer =
  proteomeTest do
    (_, buf2, buf3) <- setupBuffers
    assertJust buf3 =<< currentBufferPath
    promptInput [Mapping "k", Mapping "<cr>"] buffersMenu
    assertJust buf2 =<< currentBufferPath

test_deleteBuffer :: UnitTest
test_deleteBuffer =
  proteomeTest do
    (buf1, _, buf3) <- setupBuffers
    assertEq 3 . length =<< filterM buflisted =<< vimGetBuffers
    promptInput (Mapping <$> ["k", "d", "<esc>"]) buffersMenu
    assertEq [buf1, buf3] . catMaybes =<< traverse bufferPath =<< filterM buflisted =<< vimGetBuffers

test_wipeBuffer :: UnitTest
test_wipeBuffer =
  proteomeTest do
    (buf1, _, buf3) <- setupBuffers
    assertEq 3 . length =<< filterM buflisted =<< vimGetBuffers
    promptInput (Mapping <$> ["k", "w", "<esc>"]) buffersMenu
    assertEq [buf1, buf3] . catMaybes =<< traverse bufferPath =<< vimGetBuffers

test_deleteMultipleBuffers :: UnitTest
test_deleteMultipleBuffers =
  proteomeTest do
    (_, _, buf3) <- setupBuffers
    assertEq 3 . length =<< filterM buflisted =<< vimGetBuffers
    promptInput (Mapping <$> ["k", "<space>", "<space>", "d", "<esc>"]) buffersMenu
    assertEq [buf3] . catMaybes =<< traverse bufferPath =<< filterM buflisted =<< vimGetBuffers

test_deleteCurrentBuffer :: UnitTest
test_deleteCurrentBuffer =
  proteomeTest do
    (buf1, _, _) <- setupBuffers
    assertEq 3 . length =<< filterM buflisted =<< vimGetBuffers
    promptInput (Mapping <$> ["d", "d", "<esc>"]) buffersMenu
    assertEq [buf1] . catMaybes =<< traverse bufferPath =<< filterM buflisted =<< vimGetBuffers

test_currentBufferPosition :: UnitTest
test_currentBufferPosition =
  proteomeTest do
    (_, _, buf3) <- setupBuffers
    Settings.update Settings.buffersCurrentLast True
    bufs <- buffers
    assertJust (pathText buf3) (ListedBuffer.name . MenuItem.meta <$> last bufs)

test_buffers :: TestTree
test_buffers =
  testGroup "buffers menu" [
    unitTest "show relative paths" test_bufferPath,
    unitTest "load a buffer" test_loadBuffer,
    unitTest "delete a buffer" test_deleteBuffer,
    unitTest "wipe a buffer" test_wipeBuffer,
    unitTest "delete multiple buffers" test_deleteMultipleBuffers,
    unitTest "delete the current buffer" test_deleteCurrentBuffer,
    unitTest "show the current buffer as the last entry" test_currentBufferPosition
  ]
