{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} TagsSpec
import {-@ HTF_TESTS @-} ResolveSpec
import {-@ HTF_TESTS @-} PersistLoadSpec
import {-@ HTF_TESTS @-} PersistStoreSpec
import {-@ HTF_TESTS @-} AddSpec
import {-@ HTF_TESTS @-} MultiSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = htfMain htf_importedTests
