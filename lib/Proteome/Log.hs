module Proteome.Log(
  debug,
) where

import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Log as R (debug)

debug :: String -> Ribo e ()
debug = R.debug "proteome"
