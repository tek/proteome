module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationSpec, integrationSpecDef)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)

import Config (defaultTestConfig, defaultTestConfigWith, vars, withVars)
import Proteome.Data.Env (Env, Proteome)

specConfig :: TestConfig -> Env -> Proteome () -> IO ()
specConfig =
  unitSpec

spec :: Env -> Proteome () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Proteome () -> Vars -> IO ()
specWith env thunk vars =
  unitSpec (defaultTestConfigWith vars) env thunk

specWithDef :: Proteome () -> Vars -> IO ()
specWithDef =
  specWith def

specDef :: Proteome () -> IO ()
specDef thunk =
  specWithDef thunk def

withTmux :: Proteome () -> TmuxNative -> Proteome ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = fail "no socket in test tmux"

tmuxSpec :: Proteome () -> IO ()
tmuxSpec =
  Ribosome.tmuxSpec defaultTestConfig def

tmuxGuiSpec :: Proteome () -> IO ()
tmuxGuiSpec =
  Ribosome.tmuxGuiSpec defaultTestConfig def

integrationSpec ::
  NvimE e m =>
  MonadIO m =>
  RpcHandler e env m =>
  ReportError e =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  m () ->
  IO ()
integrationSpec reconf env thunk = do
  conf <- reconf defaultTestConfig
  Ribosome.integrationSpec conf env thunk

integrationSpecDef ::
  NvimE e m =>
  MonadIO m =>
  RpcHandler e env m =>
  ReportError e =>
  Plugin env ->
  m () ->
  IO ()
integrationSpecDef =
  integrationSpec withVars
