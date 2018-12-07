module Proteome.Test.Functional(
  spec,
  specWith,
) where

import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Embed (Vars)
import Ribosome.Test.Functional (functionalSpec)
import Proteome.Test.Config (defaultTestConfig, defaultTestConfigWith)

spec :: Ribo () () -> IO ()
spec = functionalSpec defaultTestConfig

specWith :: Vars -> Ribo () () -> IO ()
specWith vars = functionalSpec $ defaultTestConfigWith vars
