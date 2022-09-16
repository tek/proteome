module Proteome.Plugin where

import Conc (ConcStack, Lock, Restoration, interpretAtomic, interpretLockReentrant, withAsync_)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  BootError,
  Event,
  Execution (Async, Sync),
  Persist,
  PersistError,
  PluginName,
  Reports,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  completeBuiltin,
  interpretPersist,
  interpretPersistPath,
  logReport,
  resumeLogReport,
  rpc,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  runNvimPluginIO,
  )
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Effect.PersistPath (PersistPath)
import Ribosome.Host.Data.Report (LogReport)
import Ribosome.Menu (Filter, MenuFilter, Menus, NvimMenus, defaultFilter, interpretMenuLoops, interpretMenus)
import Ribosome.Menu.Data.FilterMode (FilterMode)

import Proteome.Add (AddState, proAdd, proAddCmd, proAddMenu)
import Proteome.BufEnter (Mru, bufEnter)
import Proteome.Buffers (BuffersState, proBuffers)
import Proteome.Config (proReadConfig)
import Proteome.Data.Env (Env)
import Proteome.Data.FilesState (FilesState)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Diag (proDiag)
import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Files (proFiles)
import Proteome.Grep (GrepState, proGrep, proGrepIn, proGrepList, proGrepOpt, proGrepOptIn)
import Proteome.Grep.Replace (proReplaceQuit, proReplaceSave)
import Proteome.Init (proLoad, proLoadAfter, projectConfig, projectConfigAfter, resolveAndInitMain)
import Proteome.PersistBuffers (LoadBuffersLock, StoreBuffersLock, loadBuffers)
import Proteome.Project.Activate (proNext, proPrev)
import Proteome.Quit (proQuit)
import Proteome.Save (proSave)
import Proteome.Tags (TagsLock, proTags)

type ProteomeStack =
  [
    AtomicState Env,
    Lock @@ LoadBuffersLock,
    Lock @@ StoreBuffersLock,
    Lock @@ TagsLock,
    Lock @@ Mru
  ]

type ProteomeProdStack =
  [
    Persist PersistBuffers !! PersistError,
    PersistPath !! PersistPathError,
    Menus AddState,
    Menus BuffersState,
    Menus GrepState,
    Menus FilesState,
    MenuFilter (FilterMode Filter)
  ] ++ NvimMenus ++ ProteomeStack

handlers ::
  Members ProteomeProdStack r =>
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, Reports, Reader PluginName] r =>
  Members ConcStack r =>
  Members [DataLog LogReport, ChronosTime, Log, Mask Restoration, Race, Resource, Async, Embed IO, Final IO] r =>
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
  rpc "ProGrepIn" Async proGrepIn
  <>
  rpc "ProGrepOpt" Async proGrepOpt
  <>
  rpc "ProGrepOptIn" Async proGrepOptIn
  <>
  rpc "ProReplaceSave" Async proReplaceSave
  <>
  rpc "ProReplaceQuit" Async proReplaceQuit
  <>
  rpc "ProBuffers" Async proBuffers
  <>
  rpc "ProFiles" Async proFiles
  <>
  rpc "ProPrev" Async proPrev
  <>
  rpc "ProNext" Async proNext
  <>
  rpc "ProReadConfig" Async proReadConfig
  <>
  rpc "ProLoad" Async proLoad
  <>
  rpc "ProLoadAfter" Async proLoadAfter
  <>
  [
    rpcFunction "ProGrepList" Sync proGrepList,
    rpcFunction "ProAdd" Async proAdd,
    rpcCommand "ProAdd" Async proAddCmd,
    rpcCommand "ProAddMenu" Async proAddMenu,
    completeBuiltin "file" (rpcCommand "ProMove" Async proMove),
    completeBuiltin "file" (rpcCommand "ProCopy" Async proCopy),
    rpcCommand "ProRemove" Async proRemove,
    rpcAutocmd "BufEnter" Async "BufEnter" def bufEnter,
    rpcAutocmd "ProSave" Async "BufWritePost" def proSave,
    -- If this is Async, Neovim quits before the handler has run
    rpcAutocmd "ProQuit" Sync "VimLeave" def proQuit
  ]

resolveError ::
  Member (DataLog LogReport) r =>
  Sem (Stop ResolveError : r) () ->
  Sem r ()
resolveError sem =
  runStop sem >>= \case
    Left e ->
      logReport e
    Right () ->
      unit

prepare ::
  Members [Persist PersistBuffers !! PersistError, Log, Resource, Embed IO] r =>
  Members [AtomicState Env, Settings !! SettingError, Rpc !! RpcError, Lock @@ LoadBuffersLock, DataLog LogReport] r =>
  Sem r ()
prepare = do
  resolveError $ resumeLogReport @Settings $ resumeLogReport @Rpc do
    resolveAndInitMain
  resumeLogReport @(Persist _) (resumeLogReport @Rpc loadBuffers)
  resumeLogReport @Rpc projectConfig
  resumeLogReport @Rpc projectConfigAfter

interpretProteomeStack ::
  Members [Race, Resource, Mask Restoration, Embed IO] r =>
  InterpretersFor ProteomeStack r
interpretProteomeStack =
  interpretLockReentrant . untag .
  interpretLockReentrant . untag .
  interpretLockReentrant . untag .
  interpretLockReentrant . untag .
  interpretAtomic def

interpretProteomeProdStack ::
  Member (EventConsumer eres Event) r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Reader PluginName, DataLog LogReport] r =>
  Members [Error BootError, Race, Log, Resource, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor ProteomeProdStack r
interpretProteomeProdStack =
  interpretProteomeStack .
  interpretMenus .
  defaultFilter .
  interpretMenuLoops .
  interpretMenuLoops .
  interpretMenuLoops .
  interpretMenuLoops .
  interpretPersistPath True .
  interpretPersist "buffers" .
  withAsync_ prepare

proteome :: IO ()
proteome =
  runNvimPluginIO @ProteomeProdStack "proteome" interpretProteomeProdStack handlers
