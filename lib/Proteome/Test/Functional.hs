module Proteome.Test.Functional(
  embeddedSpec,
) where

import Neovim (Neovim)
import Ribosome.Test.Functional (TestConfig)
import qualified Ribosome.Test.Functional as R (embeddedSpec, defaultTestConfig)

defaultTestConfig :: TestConfig
defaultTestConfig = R.defaultTestConfig "Proteome"

embeddedSpec :: Neovim () () -> IO ()
embeddedSpec = R.embeddedSpec defaultTestConfig
