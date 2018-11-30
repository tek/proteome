module Proteome.Log(
  debug,
  info,
) where

import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Log as R (debug, info)

debug :: String -> Ribo e ()
debug = R.debug "proteome"

info :: String -> Ribo e ()
info = R.info "proteome"
