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
  function',
  autocmd,
  Neovim,
  StartupConfig,
  NeovimConfig,
  NeovimPlugin,
  Synchronous(..),
  wrapPlugin,
  acmdGroup
  )
import Ribosome.Data.Ribosome (Ribosome, newRibosome)
import Proteome.Init (proteomePoll, proteomeStage1, proteomeStage2, proteomeStage4, initialize)
import Proteome.Data.Env (Env)
import Proteome.Add (proAdd)
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
      $(function' 'proAdd) Async,
      $(function' 'proSave) Async,
      $(function' 'proTags) Async,
      $(function' 'proReadConfig) Sync,
      $(autocmd 'bufEnter) "BufEnter" def { acmdGroup = Just "proteome" }
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  env' <- newRibosome "proteome" env
  wrapPlugin $ plugin' env'
