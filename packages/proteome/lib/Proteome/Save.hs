module Proteome.Save where

import Conc (Lock)
import Ribosome (Errors, Handler, HostError, PersistError, Rpc, RpcError, SettingError, Settings, resumeHandlerError)
import Ribosome.Effect.Persist (Persist)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.PersistBuffers (StoreBuffersLock, storeBuffers)
import Proteome.Tags (TagsLock, proTags)

proSave ::
  Member (Persist PersistBuffers !! PersistError) r =>
  Members [Settings !! SettingError, AtomicState Env, Lock @@ TagsLock, DataLog HostError] r =>
  Members [Lock @@ StoreBuffersLock, AtomicState Env, Rpc !! RpcError, Errors, Resource, Log, Async, Embed IO] r =>
  Handler r ()
proSave = do
  proTags
  resumeHandlerError @Rpc $ resumeHandlerError @(Persist _) $ storeBuffers
