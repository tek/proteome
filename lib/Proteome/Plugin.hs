{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Proteome.Plugin(
  plugin,
)
where

import Data.Default.Class (Default(def))
import Neovim (
  CommandOption(CmdBang),
  Neovim,
  NeovimConfig,
  NeovimPlugin,
  Plugin(..),
  StartupConfig,
  Synchronous(..),
  autocmd,
  command,
  command',
  function,
  function',
  wrapPlugin,
  )
import Ribosome.Control.Ribosome (Ribosome)
import UnliftIO.STM (TVar)

import Proteome.Add (proAdd, proAddCmd)
import Proteome.BufEnter (bufEnter)
import Proteome.Config (proReadConfig)
import Proteome.Data.Env (Env)
import Proteome.Diag (proDiag)
import Proteome.Init (initialize, proteomePoll, proteomeStage1, proteomeStage2, proteomeStage4)
import Proteome.Project.Activate (proNext, proPrev)
import Proteome.Save (proSave)
import Proteome.Tags (proTags)

plugin' :: Ribosome (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = env,
    exports = [
      $(function' 'proteomePoll) Sync,
      $(function' 'proteomeStage1) Sync,
      $(function' 'proteomeStage2) Sync,
      $(function' 'proteomeStage4) Sync,
      $(function "ProAddProject" 'proAdd) Async,
      $(command "ProAdd" 'proAddCmd) [CmdBang],
      $(function' 'proSave) Async,
      $(function' 'proTags) Async,
      $(command' 'proNext) [],
      $(command' 'proPrev) [],
      $(function' 'proReadConfig) Sync,
      $(command' 'proDiag) [],
      $(autocmd 'bufEnter) "BufEnter" def
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
