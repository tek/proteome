module Proteome.Test.Config(
  defaultTestConfigWith,
  defaultTestConfig,
) where

import Ribosome.Test.Embed (TestConfig, Vars)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "proteome"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "proteome"
