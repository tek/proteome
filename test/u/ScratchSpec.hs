{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ScratchSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Test.Framework
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Scratch (showInScratch)
import Proteome.Data.Proteome (Proteome)
import Proteome.Test.Unit (specWithDef)
import Config (vars)

target :: [String]
target = ["line 1", "line 2"]

scratchSpec :: Proteome ()
scratchSpec = do
  _ <- showInScratch target (ScratchOptions False True (Just 0) False "buffi")
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_scratch :: IO ()
test_scratch =
  vars >>= specWithDef scratchSpec