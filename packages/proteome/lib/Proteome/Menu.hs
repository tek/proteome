module Proteome.Menu where

import Ribosome (HandlerError, HandlerTag, Rpc, handlerError)
import Ribosome.Menu (MenuResult (Aborted, Error, NoAction, Success))

handleResult ::
  Members [Rpc, Stop HandlerError] r =>
  HandlerTag ->
  (a -> Sem r ()) ->
  MenuResult a ->
  Sem r ()
handleResult htag handle = \case
  Success a ->
    handle a
  Aborted ->
    unit
  NoAction ->
    unit
  Error e ->
    handlerError (fromText e) htag
