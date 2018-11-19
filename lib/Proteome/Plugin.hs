{-# LANGUAGE TemplateHaskell #-}

module Proteome.Plugin(
  plugin
)
where

import Neovim

import Proteome.Init (initialize)
import Proteome.Add (proAdd, proteomePoll)
import Proteome.Config (proReadConfig)

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin
    Plugin {
      environment = env,
      exports = [
        $(function "ProteomePoll" 'proteomePoll) Sync,
        $(function "ProAdd" 'proAdd) Async,
        $(function' 'proReadConfig) Async
      ]
    }
