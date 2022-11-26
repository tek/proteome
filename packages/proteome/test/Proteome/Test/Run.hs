module Proteome.Test.Run where

import Conc (interpretAtomic)
import Log (Severity (Debug, Trace))
import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest)
import Ribosome (
  BootError,
  HostConfig,
  Persist,
  PersistError,
  PersistPath,
  PersistPathError,
  PluginConfig (PluginConfig),
  Report,
  interpretPersist,
  interpretPersistPathAt,
  setStderr,
  )
import qualified Ribosome.Settings as Settings
import Ribosome.Test (EmbedStackWith, TestConfig (TestConfig), testHandler, testPluginConf)

import Proteome.Data.Env (Env)
import Proteome.Data.PersistBuffers (PersistBuffers)
import Proteome.Data.ProjectConfig (baseDirs)
import Proteome.Plugin (ProteomeStack, interpretProteomeStack)
import qualified Proteome.Settings as Settings

type ProteomeTestStack =
  AtomicState Env : ProteomeStack

type ProteomeTest =
  Stop Report : EmbedStackWith ProteomeTestStack

interpretPersistTest ::
  Members [Error BootError, Test, Log, Embed IO] r =>
  InterpretersFor [Persist PersistBuffers !! PersistError, PersistPath !! PersistPathError] r
interpretPersistTest test = do
  persistDir <- Test.tempDir [reldir|persist|]
  interpretPersistPathAt True persistDir (interpretPersist "buffers" test)

interpretProteomeStackTest ::
  Members [Race, Resource, Mask, Embed IO] r =>
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
  testPluginConf @ProteomeTestStack testConf interpretProteomeStackTest mempty $ testHandler do
      projects <- Test.fixturePath [reldir|projects|]
      mainDir <- Test.fixturePath [reldir|projects/haskell/flagellum|]
      Settings.update Settings.projectConfig def { baseDirs = [projects] }
      Settings.update Settings.mainProjectDir mainDir
      test
  where
    testConf =
      TestConfig False (PluginConfig "proteome" conf unit)

proteomeTest ::
  HasCallStack =>
  Sem ProteomeTest () ->
  UnitTest
proteomeTest =
  proteomeTestConf def

proteomeTestDebug ::
  HasCallStack =>
  Sem ProteomeTest () ->
  UnitTest
proteomeTestDebug =
  proteomeTestConf (setStderr Debug def)

proteomeTestTrace ::
  HasCallStack =>
  Sem ProteomeTest () ->
  UnitTest
proteomeTestTrace =
  proteomeTestConf (setStderr Trace def)
