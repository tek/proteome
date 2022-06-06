module Proteome.Test.ConfigTest where

import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq)
import Ribosome.Api (nvimGetVar, nvimSetCwd)
import Ribosome.Api.Option (rtpCat)
import Ribosome (pathText)
import Ribosome.Test (assertWait, testError)

import Proteome.Config (proReadConfig)
import Proteome.Init (resolveAndInitMain)
import Proteome.Test.Run (proteomeTest)

test_config :: UnitTest
test_config = do
  proteomeTest do
    rtp <- Test.fixturePath [reldir|rtp|]
    rtpCat (pathText rtp)
    dir <- Test.fixturePath [reldir|projects/haskell/flagellum|]
    nvimSetCwd dir
    testError resolveAndInitMain
    proReadConfig
    assertWait (nvimGetVar "flag") (assertEq @Int 13)
