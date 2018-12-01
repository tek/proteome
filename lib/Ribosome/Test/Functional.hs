module Ribosome.Test.Functional(
  startPlugin,
  fSpec,
  embeddedSpec,
  TestConfig (..),
  defaultTestConfig,
  defaultTestConfigWith,
  Vars(..),
) where

import Control.Monad.IO.Class
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Neovim
import Neovim.Test (testWithEmbeddedNeovim, Seconds(..))
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Ribosome.Api.Option (rtpCat)
import Ribosome.Test.Exists (waitForPlugin)

newtype Vars = Vars [(String, Object)]

data TestConfig =
  TestConfig {
    pluginName :: String,
    extraRtp :: String,
    logDir :: FilePath,
    variables :: Vars
  }

defaultTestConfigWith :: String -> Vars -> TestConfig
defaultTestConfigWith name vars = TestConfig name "test/f/fixtures/rtp" "test/f/temp/log" vars

defaultTestConfig :: String -> TestConfig
defaultTestConfig name = defaultTestConfigWith name (Vars [])

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

startPlugin :: TestConfig -> Neovim env ()
startPlugin (TestConfig name rtp logPath' (Vars vars)) = do
  liftIO $ createDirectoryIfMissing True (takeDirectory logPath')
  rtpCat rtp
  _ <- traverse (uncurry vim_set_var') vars
  cmd <- jobstart $ "stack run -- -l " ++ logPath' ++ "-spec -v INFO"
  vim_command' cmd
  waitForPlugin name 0.1 3

fSpec :: TestConfig -> Neovim env () -> Neovim env ()
fSpec conf spec = startPlugin conf >> spec

embeddedSpec :: TestConfig -> Ribo () () -> IO ()
embeddedSpec conf spec = do
  testWithEmbeddedNeovim Nothing (Seconds 5) (Ribosome (pluginName conf) ()) $ fSpec conf spec
