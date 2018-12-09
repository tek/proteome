{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} TagsSpec
import {-@ HTF_TESTS @-} ResolveSpec
import {-@ HTF_TESTS @-} PersistBuffersSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = htfMain htf_importedTests
