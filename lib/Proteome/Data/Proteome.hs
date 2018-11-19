module Proteome.Data.Proteome(
  Proteome
) where

import Neovim
import UnliftIO.STM (TVar)
import Proteome.Data.Env

type Proteome a = Neovim (TVar Env) a
