module Ribosome.Data.Ribo(
  Ribo,
  riboState,
  riboInspect,
) where

import UnliftIO.STM (TVar, readTVar, atomically)
import Neovim (Neovim, ask)
import Ribosome.Data.Ribosome

type Ribo e = Neovim (Ribosome e)

riboState :: Ribo (TVar e) e
riboState = do
  Ribosome _ t <- ask
  atomically $ readTVar t

riboInspect :: (e -> a) -> Ribo (TVar e) a
riboInspect f = fmap f riboState
