module Ribosome.Test.Functional(
  startPlugin,
  fSpec,
  embeddedSpec,
  TestConfig (..),
  defaultTestConfig,
) where

import Control.Monad.IO.Class
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Neovim
import Neovim.Test (testWithEmbeddedNeovim, Seconds(..))
import Ribosome.Api.Option (rtpCat)
import Ribosome.Test.Exists (waitForPlugin)

data TestConfig =
  TestConfig {
    pluginName :: String,
    extraRtp :: String,
    logDir :: FilePath
  }

defaultTestConfig :: String -> TestConfig
defaultTestConfig name = TestConfig name "test/f/fixtures/rtp" "test/f/temp/log"

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

startPlugin :: TestConfig -> Neovim env ()
startPlugin (TestConfig name rtp logPath') = do
  liftIO $ createDirectoryIfMissing True (takeDirectory logPath')
  rtpCat rtp
  cmd <- jobstart $ "stack run -- -l " ++ logPath' ++ "-spec -v DEBUG"
  vim_command' cmd
  waitForPlugin name 0.1 3

fSpec :: TestConfig -> Neovim env () -> Neovim env ()
fSpec conf spec = startPlugin conf >> spec

embeddedSpec :: TestConfig -> Neovim () () -> IO ()
embeddedSpec conf spec = do
  testWithEmbeddedNeovim Nothing (Seconds 5) () (fSpec conf spec)
