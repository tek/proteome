module Proteome.Test.Functional(
  embeddedSpec,
) where

import Neovim (Neovim)
import qualified Ribosome.Test.Functional as R (embeddedSpec)

embeddedSpec :: Neovim () () -> IO ()
embeddedSpec = R.embeddedSpec "Proteome"
