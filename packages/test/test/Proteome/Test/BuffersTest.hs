module Proteome.Test.BuffersTest where

import Control.Lens (view)
import Data.MonoTraversable (lastMay)
import Hedgehog ((===))
import Ribosome.Api.Buffer (bufferForFile, buflisted, currentBufferName, edit)
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Run (noPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Nvim.Api.IO (bufferGetName, vimGetBuffers)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (fixture)
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (serial)
import Test.Tasty (TestTree, testGroup)

import Proteome.Buffers (buffers, buffersWith)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (buffers)
import qualified Proteome.Data.ListedBuffer as ListedBuffer (name)
import qualified Proteome.Settings as Settings (buffersCurrentLast)
import Proteome.Test.Unit (testDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInput chars' =
  PromptInput \ _ ->
    serial (Streamly.nilM (sleep 1)) (Streamly.fromList (PromptInputEvent.Character <$> chars'))

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

setupBuffers :: Proteome (Text, Text, Text)
setupBuffers = do
  buf1 <- fixture "buffers/buf1"
  buf2 <- fixture "buffers/buf2"
  buf3 <- fixture "buffers/buf3"
  edit buf1
  edit buf2
  edit buf3
  setL @Env Env.buffers . catMaybes =<< traverse bufferForFile [toText buf3, toText buf2, toText buf1]
  return (toText buf1, toText buf2, toText buf3)

test_loadBuffer :: UnitTest
test_loadBuffer =
  testDef do
    (_, buf2, buf3) <- lift setupBuffers
    (toText buf3 ===) =<< currentBufferName
    buffersWith (promptConfig ["k", "cr"])
    (toText buf2 ===) =<< currentBufferName

test_deleteBuffer :: UnitTest
test_deleteBuffer =
  testDef do
    (buf1, _, buf3) <- lift setupBuffers
    (3 ===) . length =<< filterM buflisted =<< vimGetBuffers
    buffersWith (promptConfig ["k", "d", "esc"])
    ([buf1, buf3] ===) =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_wipeBuffer :: UnitTest
test_wipeBuffer =
  testDef do
    (buf1, _, buf3) <- lift setupBuffers
    (3 ===) . length =<< filterM buflisted =<< vimGetBuffers
    buffersWith (promptConfig ["k", "w", "esc"])
    ([buf1, buf3] ===) =<< traverse bufferGetName =<< vimGetBuffers

test_deleteMultipleBuffers :: UnitTest
test_deleteMultipleBuffers =
  testDef do
    (_, _, buf3) <- lift setupBuffers
    (3 ===) . length =<< filterM buflisted =<< vimGetBuffers
    buffersWith (promptConfig ["k", "space", "space", "d", "esc"])
    ([buf3] ===) =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_deleteCurrentBuffer :: UnitTest
test_deleteCurrentBuffer =
  testDef do
    (buf1, _, _) <- lift setupBuffers
    (3 ===) . length =<< filterM buflisted =<< vimGetBuffers
    buffersWith (promptConfig ["d", "d", "esc"])
    ([buf1] ===) =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_currentBufferPosition :: UnitTest
test_currentBufferPosition =
  testDef do
    (_, _, buf3) <- lift setupBuffers
    updateSetting Settings.buffersCurrentLast True
    bufs <- buffers
    Just buf3 === (view (MenuItem.meta . ListedBuffer.name) <$> lastMay bufs)

test_buffers :: TestTree
test_buffers =
  testGroup "buffers menu" [
    unitTest "load a buffer" test_loadBuffer,
    unitTest "delete a buffer" test_deleteBuffer,
    unitTest "wipe a buffer" test_wipeBuffer,
    unitTest "delete multiple buffers" test_deleteMultipleBuffers,
    unitTest "delete the current buffer" test_deleteCurrentBuffer,
    unitTest "show the current buffer as the last entry" test_currentBufferPosition
  ]
