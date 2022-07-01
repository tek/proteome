module Proteome.Test.Run where

import Conc (interpretAtomic, interpretSyncAs)
import Log (Severity (Debug))
import Path (Abs, Dir, Path, reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest)
import Ribosome (
  BootError,
  HandlerError,
  HostConfig,
  PluginConfig (PluginConfig),
  Rpc,
  RpcError,
  SettingError,
  Settings,
  interpretPersist,
  interpretPersistPathAt,
  noHandlers,
  setStderr,
  )
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostError (HostError)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (StackWith, TestConfig (TestConfig), runEmbedTest, testHandler, testPluginEmbed)

import Proteome.BufEnter (MruLock (MruLock))
import Proteome.Data.ProjectConfig (baseDirs)
import Proteome.PersistBuffers (LoadBuffersLock (LoadBuffersLock), StoreBuffersLock (StoreBuffersLock))
import Proteome.Plugin (ProteomeStack)
import qualified Proteome.Settings as Settings
import Proteome.Tags (TagsLock (TagsLock))

type ProteomeTestStack =
  Stop HandlerError : StackWith ProteomeStack

interpretProteomeStackTest ::
  Members [Reader PluginName, DataLog HostError] r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  Path Abs Dir ->
  InterpretersFor ProteomeStack r
interpretProteomeStackTest persistDir =
  interpretSyncAs MruLock .
  interpretSyncAs TagsLock .
  interpretSyncAs StoreBuffersLock .
  interpretSyncAs LoadBuffersLock .
  interpretPersistPathAt True persistDir .
  interpretPersist "buffers" .
  interpretAtomic def

proteomeTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem ProteomeTestStack () ->
  UnitTest
proteomeTestConf conf test =
  runEmbedTest (TestConfig False (PluginConfig "proteome" conf)) do
    persistDir <- Test.tempDir [reldir|persist|]
    interpretProteomeStackTest persistDir $ noHandlers $ testPluginEmbed $ testHandler do
      projects <- Test.fixturePath [reldir|projects|]
      mainDir <- Test.fixturePath [reldir|projects/haskell/flagellum|]
      Settings.update Settings.projectConfig def { baseDirs = [projects] }
      Settings.update Settings.mainProjectDir mainDir
      test

proteomeTest ::
  HasCallStack =>
  Sem ProteomeTestStack () ->
  UnitTest
proteomeTest =
  proteomeTestConf def

proteomeTestDebug ::
  HasCallStack =>
  Sem ProteomeTestStack () ->
  UnitTest
proteomeTestDebug =
  proteomeTestConf (setStderr Debug def)
