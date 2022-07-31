module Proteome.Save where

import Conc (Lock)
import Ribosome (Reports, Handler, LogReport, PersistError, Rpc, RpcError, SettingError, Settings, resumeReport)
import Ribosome.Effect.Persist (Persist)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.PersistBuffers (StoreBuffersLock, storeBuffers)
import Proteome.Tags (TagsLock, proTags)

proSave ::
  Member (Persist PersistBuffers !! PersistError) r =>
  Members [Settings !! SettingError, AtomicState Env, Lock @@ TagsLock, DataLog LogReport] r =>
  Members [Lock @@ StoreBuffersLock, AtomicState Env, Rpc !! RpcError, Reports, Resource, Log, Async, Embed IO] r =>
  Handler r ()
proSave = do
  proTags
  resumeReport @Rpc $ resumeReport @(Persist _) $ storeBuffers
