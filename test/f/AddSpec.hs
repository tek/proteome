{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddSpec(
  htf_thisModulesTests
) where

import qualified Data.Map as Map (fromList)
import Test.Framework
import Neovim (vim_call_function', vim_command', toObject)
import Ribosome.Data.Ribo (Ribo)
import Proteome.Test.Functional (specWith)
import Config (vars)

addSpec :: Ribo env ()
addSpec = do
  _ <- vim_call_function' "ProAdd" [toObject $ Map.fromList [("name", "bar"), ("tpe", "foo")]]
  return ()

test_add :: IO ()
test_add = do
  v <- vars
  specWith v addSpec

addCommandSpec :: Ribo env ()
addCommandSpec = do
  _ <- vim_command' "ProAdd! foo/bar"
  return ()

test_addCommand :: IO ()
test_addCommand = do
  v <- vars
  specWith v addCommandSpec
