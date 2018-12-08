module Ribosome.Data.Ribo(
  Ribo,
  riboState,
  riboInspect,
  riboModify,
) where

import Control.Concurrent.STM.TVar (modifyTVar)
import UnliftIO.STM (TVar, atomically, readTVarIO)
import Neovim (Neovim, ask)
import Ribosome.Data.Ribosome

type Ribo e = Neovim (Ribosome e)

riboState :: Ribo (TVar e) e
riboState = do
  Ribosome _ t <- ask
  readTVarIO t

riboInspect :: (e -> a) -> Ribo (TVar e) a
riboInspect f = fmap f riboState

riboModify :: (e -> e) -> Ribo (TVar e) ()
riboModify f = do
  Ribosome _ t <- ask
  atomically $ modifyTVar t f
