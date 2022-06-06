module Proteome.Save where

import Ribosome (Errors, Handler, HostError, PersistError, Rpc, RpcError, SettingError, Settings, resumeHandlerError)
import Ribosome.Effect.Persist (Persist)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.PersistBuffers (StoreBuffersLock, storeBuffers)
import Proteome.Tags (TagsLock, proTags)

proSave ::
  Member (Persist PersistBuffers !! PersistError) r =>
  Members [Settings !! SettingError, AtomicState Env, Sync TagsLock, DataLog HostError] r =>
  Members [Sync StoreBuffersLock, AtomicState Env, Rpc !! RpcError, Errors, Resource, Log, Async, Embed IO] r =>
  Handler r ()
proSave = do
  proTags
  resumeHandlerError @Rpc $ resumeHandlerError @(Persist _) $ storeBuffers
