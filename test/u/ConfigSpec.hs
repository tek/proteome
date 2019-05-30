{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec(
  htf_thisModulesTests
) where

import Config (vars)
import Control.Monad.IO.Class (liftIO)
import Data.MessagePack (Object(ObjectInt))
import Neovim (vim_call_function', vim_get_var')
import Proteome.Test.Functional (specWith)
import Ribosome.Control.Ribo (Ribo)
import Test.Framework

configSpec :: Ribo env ()
configSpec = do
  _ <- vim_call_function' "ProReadConfig" []
  value <- vim_get_var' "flag"
  liftIO $ assertEqual value (ObjectInt 13)

test_config :: IO ()
test_config =
  vars >>= specWith configSpec
