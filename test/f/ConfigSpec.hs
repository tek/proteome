{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Neovim
import Neovim.Test
import Ribosome.Test.Functional (fSpec)

configSpec :: Neovim env ()
configSpec =
  do
    e <- vim_call_function "ProReadConfig" []
    liftIO (print e)
    return ()

test_config :: IO ()
test_config =
  testWithEmbeddedNeovim Nothing (Seconds 5) () (fSpec "Proteome" configSpec)
