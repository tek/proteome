module Proteome.Log(
  debug,
  info,
  debugS,
  infoS,
) where

import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Log as R (debug, info)

debug :: String -> Ribo e ()
debug = R.debug "proteome"

info :: String -> Ribo e ()
info = R.info "proteome"

debugS :: Show a => a -> Ribo e ()
debugS = R.debug "proteome"

infoS :: Show a => a -> Ribo e ()
infoS = R.info "proteome"