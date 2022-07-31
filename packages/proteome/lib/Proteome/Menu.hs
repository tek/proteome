module Proteome.Menu where

import Ribosome (Report, Rpc)
import Ribosome.Menu (MenuResult (Aborted, Error, NoAction, Success))

handleResult ::
  Members [Rpc, Stop Report] r =>
  (a -> Sem r ()) ->
  MenuResult a ->
  Sem r ()
handleResult handle = \case
  Success a ->
    handle a
  Aborted ->
    unit
  NoAction ->
    unit
  Error e ->
    stop (fromText e)
