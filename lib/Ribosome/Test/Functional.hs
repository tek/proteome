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
import Control.Exception (finally)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory)
-- import System.Console.Pretty (color, style, Color(..), Style(..))
import System.Console.ANSI
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
    logPath :: FilePath,
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

logFile :: TestConfig -> FilePath
logFile conf = logPath conf ++ "-spec"

startPlugin :: TestConfig -> Neovim env ()
startPlugin conf @ (TestConfig name rtp logPath' (Vars vars)) = do
  liftIO $ createDirectoryIfMissing True (takeDirectory logPath')
  liftIO $ removePathForcibly (logFile conf)
  rtpCat rtp
  _ <- traverse (uncurry vim_set_var') vars
  cmd <- jobstart $ "stack run -- -l " ++ (logFile conf) ++ " -v INFO"
  vim_command' cmd
  waitForPlugin name 0.1 3

fSpec :: TestConfig -> Neovim env () -> Neovim env ()
fSpec conf spec = startPlugin conf >> spec

unsafeEmbeddedSpec :: TestConfig -> Ribo () () -> IO ()
unsafeEmbeddedSpec conf spec =
  testWithEmbeddedNeovim Nothing (Seconds 5) (Ribosome (pluginName conf) ()) $ fSpec conf spec


showLog :: TestConfig -> IO ()
showLog conf = do
  output <- readFile $ logFile conf
  putStrLn ""
  setSGR [SetColor Foreground Dull Green]
  putStrLn $ "plugin output:"
  setSGR [Reset]
  _ <- traverse putStrLn (lines output)
  putStrLn ""

embeddedSpec :: TestConfig -> Ribo () () -> IO ()
embeddedSpec conf spec =
  finally (unsafeEmbeddedSpec conf spec) (showLog conf)
