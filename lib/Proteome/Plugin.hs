{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Proteome.Plugin(
  plugin,
)
where

import Data.Default.Class (Default(def))
import UnliftIO.STM (TVar)
import Neovim (
  Plugin(..),
  function,
  function',
  command,
  autocmd,
  Neovim,
  StartupConfig,
  NeovimConfig,
  NeovimPlugin,
  Synchronous(..),
  wrapPlugin,
  CommandOption(CmdBang),
  )
import Ribosome.Data.Ribosome (Ribosome)
import Proteome.Init (proteomePoll, proteomeStage1, proteomeStage2, proteomeStage4, initialize)
import Proteome.Data.Env (Env)
import Proteome.Add (proAdd, proAddCmd)
import Proteome.Config (proReadConfig)
import Proteome.Tags (proTags)
import Proteome.Save (proSave)
import Proteome.BufEnter (bufEnter)

plugin' :: Ribosome (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = env,
    exports = [
      $(function' 'proteomePoll) Sync,
      $(function' 'proteomeStage1) Async,
      $(function' 'proteomeStage2) Async,
      $(function' 'proteomeStage4) Async,
      $(function "ProAddProject" 'proAdd) Sync,
      $(command "ProAdd" 'proAddCmd) [CmdBang],
      $(function' 'proSave) Async,
      $(function' 'proTags) Async,
      $(function' 'proReadConfig) Sync,
      $(autocmd 'bufEnter) "BufEnter" def
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
