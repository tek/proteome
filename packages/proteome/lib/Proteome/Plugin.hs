module Proteome.Plugin where

import Conc (Restoration, interpretAtomic, interpretSyncAs, withAsync_)
import Log (dataLog)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  BootError,
  Errors,
  Execution (Async),
  Persist,
  PersistError,
  PluginName,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  ToErrorMessage,
  interpretPersist,
  interpretPersistPath,
  rpc,
  rpcCommand,
  rpcFunction,
  runNvimHandlersIO,
  toHandlerError,
  )
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Effect.PersistPath (PersistPath)
import Ribosome.Host.Data.HostError (HostError (HostError))

import Proteome.Add (proAdd, proAddCmd)
import Proteome.BufEnter (MruLock (MruLock))
import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Diag (proDiag)
import Proteome.Grep (proGrep)
import Proteome.Init (projectConfig, resolveAndInitMain)
import Proteome.PersistBuffers (LoadBuffersLock (LoadBuffersLock), StoreBuffersLock (StoreBuffersLock), loadBuffers)
import Proteome.Save (proSave)
import Proteome.Tags (TagsLock (TagsLock), proTags)

type ProteomeStack =
  [
    AtomicState Env,
    Persist PersistBuffers !! PersistError,
    PersistPath !! PersistPathError,
    Sync LoadBuffersLock,
    Sync StoreBuffersLock,
    Sync TagsLock,
    Sync MruLock
  ]

handlers ::
  Members ProteomeStack r =>
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, Errors, Reader PluginName] r =>
  Members [DataLog HostError, ChronosTime, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
  [RpcHandler r]
handlers =
  rpc "ProDiag" Async proDiag
  <>
  rpc "ProSave" Async proSave
  <>
  rpc "ProTags" Async proTags
  <>
  rpc "ProGrep" Async proGrep
  <>
  [
    rpcFunction "ProAdd" Async proAdd,
    rpcCommand "ProAdd" Async proAddCmd
  ]

-- rpcHandlers =
--   [
--     $(rpcHandler (cmd []) 'proGrep),
--     $(rpcHandler (cmd []) 'proGrepIn),
--     $(rpcHandler (cmd []) 'proGrepOpt),
--     $(rpcHandler (cmd []) 'proGrepOptIn),
--     $(rpcHandler sync 'proGrepList),
--     $(rpcHandler (cmd []) 'proReplaceSave),
--     $(rpcHandler (cmd []) 'proReplaceQuit),
--     $(rpcHandler (cmd []) 'proBuffers),
--     $(rpcHandler (cmd []) 'proFiles),
--     $(rpcHandler (cmd []) 'proNext),
--     $(rpcHandler (cmd []) 'proPrev),
--     $(rpcHandler (cmd [CmdBang, CmdComplete "file"]) 'proMove),
--     $(rpcHandler (cmd [CmdBang, CmdComplete "file"]) 'proCopy),
--     $(rpcHandler (cmd []) 'proRemove),
--     $(rpcHandlerDef 'proReadConfig),
--     $(rpcHandler (cmd []) 'proDiag),
--     $(rpcHandler (autocmd "BufEnter") 'bufEnter),
--     $(rpcHandler (autocmd "BufWritePost") 'proSave),
--     $(rpcHandler (autocmd "VimLeave" . sync) 'proQuit)
--   ]

logError ::
  ∀ e r .
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  e ->
  Sem r ()
logError =
  dataLog . HostError True . toHandlerError "init"

resumeLogError ::
  ∀ eff e r .
  ToErrorMessage e =>
  Members [eff !! e, DataLog HostError] r =>
  Sem (eff : r ) () ->
  Sem r ()
resumeLogError =
  resuming logError

resolveError ::
  Member (DataLog HostError) r =>
  Sem (Stop ResolveError : r) () ->
  Sem r ()
resolveError sem =
  runStop sem >>= \case
    Left e ->
      logError e
    Right () ->
      unit

prepare ::
  Members [Persist PersistBuffers !! PersistError, Log, Resource, Embed IO] r =>
  Members [AtomicState Env, Settings !! SettingError, Rpc !! RpcError, Sync LoadBuffersLock, DataLog HostError] r =>
  Sem r ()
prepare = do
  resolveError $ resumeLogError @Settings $ resumeLogError @Rpc do
    resolveAndInitMain
  resumeLogError @(Persist _) (resumeLogError @Rpc loadBuffers)
  resumeLogError @Rpc projectConfig

interpretProteomeStack ::
  Members [Reader PluginName, DataLog HostError] r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor ProteomeStack r
interpretProteomeStack sem =
  interpretSyncAs MruLock .
  interpretSyncAs TagsLock .
  interpretSyncAs StoreBuffersLock .
  interpretSyncAs LoadBuffersLock .
  interpretPersistPath True $
  interpretPersist "buffers" $
  interpretAtomic def do
    withAsync_ prepare sem

proteome :: IO ()
proteome =
  runNvimHandlersIO @ProteomeStack "proteome" interpretProteomeStack handlers
