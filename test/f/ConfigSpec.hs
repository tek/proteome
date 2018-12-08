{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Data.MessagePack (Object(ObjectInt))
import Neovim (vim_call_function, vim_get_var')
import Ribosome.Data.Ribo (Ribo)
import Proteome.Test.Functional (specWith)
import Config (vars)

configSpec :: Ribo env ()
configSpec = do
  _ <- vim_call_function "ProReadConfig" []
  value <- vim_get_var' "flag"
  liftIO $ assertEqual value (ObjectInt 1)

test_config :: IO ()
test_config = do
  v <- vars
  specWith v configSpec
