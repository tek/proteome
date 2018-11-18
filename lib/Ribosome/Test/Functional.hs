module Ribosome.Test.Functional(
  startPlugin,
  fSpec
) where

import Control.Monad.IO.Class
import System.Directory
import Neovim
import Ribosome.Test.Exists (waitForPlugin)
import Ribosome.Test.IO (nvimFail)

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('stack " ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

startPlugin :: String -> Neovim env ()
startPlugin name = do
  cmd <- jobstart $ "run -- -l /tmp/" ++ name ++ "-spec -v DEBUG"
  e <- vim_command $ cmd
  nvimFail e
  _ <- waitForPlugin name 0.1 3
  return ()

fSpec :: String -> Neovim env () -> Neovim env ()
fSpec name spec = startPlugin name >> spec
