module Proteome.Test.Run where

import Conc (Restoration, interpretAtomic)
import Log (Severity (Trace))
import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest)
import Ribosome (
  BootError,
  HandlerError,
  HostConfig,
  Persist,
  PersistError,
  PersistPath,
  PersistPathError,
  PluginConfig (PluginConfig),
  interpretPersist,
  interpretPersistPathAt,
  noHandlers,
  setStderr,
  )
import qualified Ribosome.Settings as Settings
import Ribosome.Test (EmbedStackWith, TestConfig (TestConfig), runEmbedTest, testHandler, testPluginEmbed)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.Data.ProjectConfig (baseDirs)
import Proteome.Plugin (ProteomeStack, interpretProteomeStack)
import qualified Proteome.Settings as Settings

type ProteomeTestStack =
  AtomicState Env : ProteomeStack

type ProteomeTest =
  Stop HandlerError : EmbedStackWith ProteomeTestStack

interpretPersistTest ::
  Members [Error BootError, Test, Log, Embed IO] r =>
  InterpretersFor [Persist PersistBuffers !! PersistError, PersistPath !! PersistPathError] r
interpretPersistTest test = do
  persistDir <- Test.tempDir [reldir|persist|]
  interpretPersistPathAt True persistDir (interpretPersist "buffers" test)

interpretProteomeStackTest ::
  Members [Race, Resource, Mask Restoration, Embed IO] r =>
  InterpretersFor ProteomeTestStack r
interpretProteomeStackTest =
  interpretProteomeStack .
  interpretAtomic def

proteomeTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem ProteomeTest () ->
  UnitTest
proteomeTestConf conf test =
  runEmbedTest (TestConfig False (PluginConfig "proteome" conf)) do
    interpretProteomeStackTest $ noHandlers $ testPluginEmbed $ testHandler do
      projects <- Test.fixturePath [reldir|projects|]
      mainDir <- Test.fixturePath [reldir|projects/haskell/flagellum|]
      Settings.update Settings.projectConfig def { baseDirs = [projects] }
      Settings.update Settings.mainProjectDir mainDir
      test

proteomeTest ::
  HasCallStack =>
  Sem ProteomeTest () ->
  UnitTest
proteomeTest =
  proteomeTestConf def

proteomeTestTrace ::
  HasCallStack =>
  Sem ProteomeTest () ->
  UnitTest
proteomeTestTrace =
  proteomeTestConf (setStderr Trace def)
