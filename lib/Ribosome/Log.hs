module Ribosome.Log(
  debug,
  info,
) where

import Control.Monad.IO.Class (liftIO)
import Neovim.Log (debugM, infoM)
import Ribosome.Data.Ribo (Ribo)

debug :: String -> String -> Ribo e ()
debug name message = liftIO $ debugM name message

info :: String -> String -> Ribo e ()
info name message = liftIO $ infoM name message
