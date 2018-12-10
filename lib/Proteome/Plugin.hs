{-# LANGUAGE TemplateHaskell #-}

module Proteome.Plugin(
  plugin,
)
where

import UnliftIO.STM (TVar)
import Neovim
import Ribosome.Data.Ribosome (Ribosome(Ribosome))
import Proteome.Init
import Proteome.Data.Env
import Proteome.Add (proAdd, proteomePoll)
import Proteome.Config (proReadConfig)
import Proteome.Tags (proTags)
import Proteome.Save (proSave)

plugin' :: TVar Env -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = Ribosome "proteome" env,
    exports = [
      $(function' 'proteomePoll) Sync,
      $(function' 'proteomeStage1) Async,
      $(function' 'proteomeStage2) Async,
      $(function' 'proteomeStage4) Async,
      $(function' 'proAdd) Async,
      $(function' 'proSave) Async,
      $(function' 'proTags) Async,
      $(function' 'proReadConfig) Sync
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
