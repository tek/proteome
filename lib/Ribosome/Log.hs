module Ribosome.Log(
  debug,
) where

import Control.Monad.IO.Class (liftIO)
import Neovim.Log (debugM)
import Ribosome.Data.Ribo (Ribo)

debug :: String -> String -> Ribo e ()
debug name message = liftIO $ debugM name message
