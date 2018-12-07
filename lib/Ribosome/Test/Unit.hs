module Ribosome.Test.Unit(
  unitSpec,
) where

import Neovim (Neovim)
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Embed (TestConfig(..), setupPluginEnv, unsafeEmbeddedSpec)

uSpec :: TestConfig -> Neovim env () -> Neovim env ()
uSpec conf spec = do
  setupPluginEnv conf
  spec

unitSpec :: TestConfig -> e -> Ribo e () -> IO ()
unitSpec =
  unsafeEmbeddedSpec uSpec
