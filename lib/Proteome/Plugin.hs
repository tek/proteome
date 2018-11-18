{-# LANGUAGE TemplateHaskell #-}

module Proteome.Plugin(
  plugin
)
where

import Neovim

import Proteome.Init (initialize, env)
import Proteome.Add (proAdd, proteomePoll)
import Proteome.Config (proReadConfig)

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  initialize
  wrapPlugin
    Plugin {
      environment = env,
      exports = [
        $(function' 'proteomePoll) Sync,
        $(function' 'proAdd) Async,
        $(function' 'proReadConfig) Async
      ]
    }
