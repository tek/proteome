module Proteome.Data.Proteome(
  Proteome
) where

import UnliftIO.STM (TVar)
import Ribosome.Control.Ribo (Ribo)
import Proteome.Data.Env (Env)

type Proteome a = Ribo (TVar Env) a
