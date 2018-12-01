{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Neovim (vim_call_function, vim_get_var')
import Ribosome.Data.Ribo (Ribo)
import Proteome.Test.Functional (embeddedSpecWith)
import Config (vars)

configSpec :: Ribo env ()
configSpec = do
  _ <- vim_call_function "ProReadConfig" []
  x <- vim_get_var' "flag"
  liftIO $ print x

test_config :: IO ()
test_config = do
  v <- vars
  embeddedSpecWith v configSpec
