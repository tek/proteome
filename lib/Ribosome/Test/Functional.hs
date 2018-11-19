module Ribosome.Test.Functional(
  startPlugin,
  fSpec
) where

import Control.Monad.IO.Class
import System.Directory
import Neovim
import Ribosome.Api.Option (rtpCat)
import Ribosome.Test.Exists (waitForPlugin)

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

startPlugin :: String -> Neovim env ()
startPlugin name = do
  rtpCat "test/f/fixtures/rtp"
  cmd <- jobstart $ "stack run -- -l /tmp/" ++ name ++ "-spec -v DEBUG"
  vim_command' cmd
  waitForPlugin name 0.1 3

fSpec :: String -> Neovim env () -> Neovim env ()
fSpec name spec = startPlugin name >> spec
