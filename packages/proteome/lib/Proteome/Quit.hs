module Proteome.Quit where

import Conc (Lock)
import Ribosome (Handler, PersistError, Rpc, RpcError, resumeReport)
import Ribosome.Effect.Persist (Persist)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.PersistBuffers (StoreBuffersLock, storeBuffers)

proQuit ::
  Member (Persist PersistBuffers !! PersistError) r =>
  Members [Lock @@ StoreBuffersLock, AtomicState Env, Rpc !! RpcError, Resource, Embed IO] r =>
  Handler r ()
proQuit =
  resumeReport @(Persist _) $ resumeReport @Rpc do
    storeBuffers
