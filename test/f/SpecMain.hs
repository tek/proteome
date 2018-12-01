{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} ConfigSpec
import {-@ HTF_TESTS @-} InitSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = do
  htfMain htf_importedTests
