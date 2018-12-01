module Proteome.Test.Functional(
  embeddedSpec,
  embeddedSpecWith,
) where

import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Functional (TestConfig)
import Ribosome.Test.Functional (Vars)
import qualified Ribosome.Test.Functional as R (embeddedSpec, defaultTestConfig, defaultTestConfigWith)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = R.defaultTestConfigWith "proteome"

defaultTestConfig :: TestConfig
defaultTestConfig = R.defaultTestConfig "proteome"

embeddedSpec :: Ribo () () -> IO ()
embeddedSpec = R.embeddedSpec defaultTestConfig

embeddedSpecWith :: Vars -> Ribo () () -> IO ()
embeddedSpecWith vars = R.embeddedSpec $ defaultTestConfigWith vars
