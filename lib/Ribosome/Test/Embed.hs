module Ribosome.Test.Embed(
  defaultTestConfig,
  defaultTestConfigWith,
  TestConfig (..),
  Vars(..),
  unsafeEmbeddedSpec,
  setVars,
  setupPluginEnv,
) where

import Data.Foldable (traverse_)
import Neovim (Neovim, Object, vim_set_var')
import Neovim.Test (testWithEmbeddedNeovim, Seconds(..))
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Ribosome.Api.Option (rtpCat)

type Runner env = TestConfig -> Neovim env () -> Neovim env ()

newtype Vars = Vars [(String, Object)]

data TestConfig =
  TestConfig {
    pluginName :: String,
    extraRtp :: String,
    logPath :: FilePath,
    variables :: Vars
  }

defaultTestConfigWith :: String -> Vars -> TestConfig
defaultTestConfigWith name = TestConfig name "test/f/fixtures/rtp" "test/f/temp/log"

defaultTestConfig :: String -> TestConfig
defaultTestConfig name = defaultTestConfigWith name (Vars [])

setVars :: Vars -> Neovim e ()
setVars (Vars vars) =
  traverse_ (uncurry vim_set_var') vars

setupPluginEnv :: TestConfig -> Neovim e ()
setupPluginEnv (TestConfig _ rtp _ vars) = do
  rtpCat rtp
  setVars vars

unsafeEmbeddedSpec :: Runner (Ribosome e) -> TestConfig -> e -> Ribo e () -> IO ()
unsafeEmbeddedSpec runner conf env spec =
  testWithEmbeddedNeovim Nothing (Seconds 5) (Ribosome (pluginName conf) env) $ runner conf spec
