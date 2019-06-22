{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MruSpec (htf_thisModulesTests) where

import Ribosome.Api.Buffer (edit)
import Ribosome.Nvim.Api.IO (bufferGetName, vimCommand)
import Test.Framework
import Unit (specDef)

import Proteome.BufEnter (updateBuffers)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (buffers)

setupBuffers ::
  NvimE e m =>
  MonadIO m =>
  m (Text, Text, Text)
setupBuffers = do
  buf1 <- fixture "buffers/buf1"
  buf2 <- fixture "buffers/buf2"
  buf3 <- fixture "buffers/buf3"
  edit buf1
  edit buf2
  edit buf3
  return (toText buf1, toText buf2, toText buf3)

mruSpec :: Proteome ()
mruSpec = do
  (buf1, buf2, buf3) <- setupBuffers
  edit (toString buf3)
  updateBuffers
  edit (toString buf2)
  updateBuffers
  edit (toString buf1)
  updateBuffers
  vimCommand "bdelete! 2"
  updateBuffers
  gassertEqual [buf1, buf3] =<< traverse bufferGetName =<< getL @Env Env.buffers

test_mru :: IO ()
test_mru =
  specDef mruSpec
