{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Neovim
import Proteome.Test.Functional (embeddedSpec)

configSpec :: Neovim env ()
configSpec =
  do
    e <- vim_call_function "ProReadConfig" []
    liftIO $ print e

test_config :: IO ()
test_config = embeddedSpec configSpec
