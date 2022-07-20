module Proteome.Plugin where

import Conc (Lock, Restoration, interpretAtomic, interpretLockReentrant, withAsync_)
import Log (dataLog)
import Path (Abs, File, Path)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  BootError,
  Errors,
  Execution (Async, Sync),
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
  completeBuiltin,
  interpretPersist,
  interpretPersistPath,
  rpc,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  runNvimPluginIO,
  toHandlerError,
  )
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Effect.PersistPath (PersistPath)
import Ribosome.Host.Data.HostError (HostError (HostError))
import Ribosome.Menu (
  MenuState,
  NvimRenderer,
  interpretMenuStates,
  )
import Ribosome.Menu.Interpreter.Menu (MenusIOEffects, NvimMenusIOEffects, interpretNvimMenusFinal)
import Ribosome.Menu.Interpreter.MenuRenderer (interpretMenuRendererNvim)

import Proteome.Add (proAdd, proAddCmd, proAddMenu)
import Proteome.BufEnter (Mru, bufEnter)
import Proteome.Buffers (proBuffers)
import Proteome.Config (proReadConfig)
import Proteome.Data.AddItem (AddItem)
import Proteome.Data.Env (Env)
import Proteome.Data.GrepOutputLine (GrepOutputLine)
import Proteome.Data.ListedBuffer (ListedBuffer)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Diag (proDiag)
import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Files (proFiles)
import Proteome.Grep (proGrep, proGrepIn, proGrepList, proGrepOpt, proGrepOptIn)
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
    NvimRenderer (Path Abs File) !! RpcError,
    Scoped () (MenuState (Path Abs File)),
    NvimRenderer AddItem !! RpcError,
    Scoped () (MenuState AddItem),
    NvimRenderer GrepOutputLine !! RpcError,
    Scoped () (MenuState GrepOutputLine),
    NvimRenderer ListedBuffer !! RpcError,
    Scoped () (MenuState ListedBuffer)
  ] ++ MenusIOEffects ++ NvimMenusIOEffects ++ ProteomeStack

handlers ::
  Members ProteomeProdStack r =>
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
  Members [AtomicState Env, Settings !! SettingError, Rpc !! RpcError, Lock @@ LoadBuffersLock, DataLog HostError] r =>
  Sem r ()
prepare = do
  resolveError $ resumeLogError @Settings $ resumeLogError @Rpc do
    resolveAndInitMain
  resumeLogError @(Persist _) (resumeLogError @Rpc loadBuffers)
  resumeLogError @Rpc projectConfig
  resumeLogError @Rpc projectConfigAfter

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
  Members [Rpc !! RpcError, Settings !! SettingError, Scratch !! RpcError, Reader PluginName, DataLog HostError] r =>
  Members [Error BootError, Race, Log, Resource, Mask Restoration, Async, Embed IO, Final IO] r =>
  InterpretersFor ProteomeProdStack r
interpretProteomeProdStack =
  interpretProteomeStack .
  interpretNvimMenusFinal .
  interpretMenuStates .
  interpretMenuRendererNvim .
  interpretMenuStates .
  interpretMenuRendererNvim .
  interpretMenuStates .
  interpretMenuRendererNvim .
  interpretMenuStates .
  interpretMenuRendererNvim .
  interpretPersistPath True .
  interpretPersist "buffers" .
  withAsync_ prepare

proteome :: IO ()
proteome =
  runNvimPluginIO @ProteomeProdStack "proteome" interpretProteomeProdStack handlers
