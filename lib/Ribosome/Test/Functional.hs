module Ribosome.Test.Functional(
  startPlugin,
  fSpec,
  functionalSpec,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Exception (finally)
import Data.Foldable (traverse_)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, removePathForcibly, doesFileExist)
import System.FilePath (takeDirectory)
import System.Console.ANSI (setSGR, SGR(SetColor, Reset), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(Green))
import Neovim (Neovim, vim_command')
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Exists (waitForPlugin)
import Ribosome.Test.Embed (TestConfig(..), unsafeEmbeddedSpec, setupPluginEnv)

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

logFile :: TestConfig -> FilePath
logFile conf = logPath conf ++ "-spec"

startPlugin :: TestConfig -> Neovim env ()
startPlugin conf = do
  liftIO $ createDirectoryIfMissing True (takeDirectory (logPath conf))
  liftIO $ removePathForcibly (logFile conf)
  setupPluginEnv conf
  cmd <- jobstart $ "stack run -- -l " ++ logFile conf ++ " -v INFO"
  vim_command' cmd
  waitForPlugin (pluginName conf) 0.1 3

fSpec :: TestConfig -> Neovim env () -> Neovim env ()
fSpec conf spec = startPlugin conf >> spec

showLog' :: String -> IO ()
showLog' output = do
  putStrLn ""
  setSGR [SetColor Foreground Dull Green]
  putStrLn "plugin output:"
  setSGR [Reset]
  traverse_ putStrLn (lines output)
  putStrLn ""

showLog :: TestConfig -> IO ()
showLog conf = do
  let file = logFile conf
  exists <- doesFileExist file
  when exists $ do
    output <- readFile file
    case output of
      [] -> return ()
      o -> showLog' o

functionalSpec :: TestConfig -> Ribo () () -> IO ()
functionalSpec conf spec =
  finally (unsafeEmbeddedSpec fSpec conf () spec) (showLog conf)
