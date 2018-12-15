module Proteome.Test.Functional(
  spec,
  specWith,
  fixture,
  tempDir,
  tempFile,
) where

import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Embed (Vars)
import Ribosome.Test.Functional (functionalSpec, fixture, tempDir, tempFile)
import Proteome.Test.Config (defaultTestConfig, defaultTestConfigWith)

spec :: Ribo () () -> IO ()
spec = functionalSpec defaultTestConfig

specWith :: Ribo () () -> Vars -> IO ()
specWith thunk vars = functionalSpec (defaultTestConfigWith vars) thunk
