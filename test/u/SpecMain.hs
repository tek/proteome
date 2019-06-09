{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} AddFSpec
import {-@ HTF_TESTS @-} AddSpec
import {-@ HTF_TESTS @-} ConfigSpec
import {-@ HTF_TESTS @-} CycleSpec
import {-@ HTF_TESTS @-} DiagSpec
import {-@ HTF_TESTS @-} InitSpec
import {-@ HTF_TESTS @-} MultiTagsSpec
import {-@ HTF_TESTS @-} PersistLoadSpec
import {-@ HTF_TESTS @-} PersistStoreSpec
import {-@ HTF_TESTS @-} ResolveSpec
import {-@ HTF_TESTS @-} TagsSpec

main :: IO ()
main = htfMain htf_importedTests
