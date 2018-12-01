{-# LANGUAGE TemplateHaskell #-}

module Proteome.Plugin(
  plugin
)
where

import UnliftIO.STM (TVar)
import Neovim
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Proteome.Init
import Proteome.Data.Env
import Proteome.Add (proAdd, proteomePoll)
import Proteome.Config (proReadConfig)

plugin' :: (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = Ribosome "proteome" env,
    exports = [
      $(function "ProteomePoll" 'proteomePoll) Sync,
      $(function' 'proteomeStage2) Async,
      $(function' 'proteomeStage4) Async,
      $(function "ProAdd" 'proAdd) Async,
      $(function' 'proReadConfig) Sync
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
