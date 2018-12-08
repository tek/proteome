{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ResolveSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Ribosome.File (canonicalPaths)

paths :: [FilePath]
paths = [
  "~/../test/dir",
  "~"
  ]

test_canonicalPaths :: IO ()
test_canonicalPaths = do
  canon <- canonicalPaths paths
  print canon
  assertNotEqual canon paths
