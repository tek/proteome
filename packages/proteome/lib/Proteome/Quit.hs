module Proteome.Quit where

import Ribosome (Handler, PersistError, Rpc, RpcError, resumeHandlerError)
import Ribosome.Effect.Persist (Persist)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.PersistBuffers (StoreBuffersLock, storeBuffers)

proQuit ::
  Member (Persist PersistBuffers !! PersistError) r =>
  Members [Sync StoreBuffersLock, AtomicState Env, Rpc !! RpcError, Resource, Embed IO] r =>
  Handler r ()
proQuit =
  resumeHandlerError @(Persist _) $ resumeHandlerError @Rpc do
    storeBuffers
