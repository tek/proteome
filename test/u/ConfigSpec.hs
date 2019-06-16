{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec (htf_thisModulesTests) where

import Data.MessagePack (Object(ObjectInt))
import Ribosome.Control.Ribosome (newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimGetVar)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Init (resolveAndInitMain)
import Proteome.Plugin (plugin')

configSpec :: Proteome ()
configSpec = do
  resolveAndInitMain
  () <- vimCallFunction "ProReadConfig" []
  await (gassertEqual (ObjectInt 13)) (vimGetVar "flag")

test_config :: IO ()
test_config = do
  ribo <- newRibosome "proteome" def
  integrationSpecDef (plugin' ribo) configSpec
