{-# OPTIONS_GHC -F -pgmF htfpp #-}

module BuffersSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Ribosome.Api.Buffer (bufferForFile, buflisted, currentBufferName, edit)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Nvim.Api.IO (bufferGetName, vimGetBuffers)
import Test.Framework

import Proteome.Buffers (buffersWith)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (buffers)
import Unit (specDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' =
  sleep 1 *>
  yieldMany (PromptEvent.Character <$> chars')

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer False

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

loadBufferSpec :: Proteome ()
loadBufferSpec = do
  (_, buf2, buf3) <- setupBuffers
  gassertEqual (toText buf3) =<< currentBufferName
  buffersWith (promptConfig ["k", "cr"])
  gassertEqual (toText buf2) =<< currentBufferName

test_loadBuffer :: IO ()
test_loadBuffer =
  specDef loadBufferSpec

deleteBufferSpec :: Proteome ()
deleteBufferSpec = do
  (buf1, _, buf3) <- setupBuffers
  gassertEqual 3 . length =<< filterM buflisted =<< vimGetBuffers
  buffersWith (promptConfig ["k", "d"])
  gassertEqual [buf1, buf3] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_deleteBuffer :: IO ()
test_deleteBuffer =
  specDef deleteBufferSpec

wipeBufferSpec :: Proteome ()
wipeBufferSpec = do
  (buf1, _, buf3) <- setupBuffers
  gassertEqual 3 . length =<< filterM buflisted =<< vimGetBuffers
  buffersWith (promptConfig ["k", "w"])
  gassertEqual [buf1, buf3] =<< traverse bufferGetName =<< vimGetBuffers

test_wipeBuffer :: IO ()
test_wipeBuffer =
  specDef wipeBufferSpec

deleteMultipleBuffersSpec :: Proteome ()
deleteMultipleBuffersSpec = do
  (_, _, buf3) <- setupBuffers
  gassertEqual 3 . length =<< filterM buflisted =<< vimGetBuffers
  buffersWith (promptConfig ["k", "space", "space", "d"])
  gassertEqual [buf3] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers

test_deleteMultipleBuffers :: IO ()
test_deleteMultipleBuffers =
  specDef deleteMultipleBuffersSpec
