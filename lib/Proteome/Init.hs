module Proteome.Init(
  initialize
) where

import Neovim
import UnliftIO.STM (TVar, newTVarIO)
import Proteome.Data.Env

initialize :: Neovim startupEnv (TVar Env)
initialize = newTVarIO $ Env Nothing []
