module Ribosome.Data.Ribosome(
  Ribosome (..),
  newRibosome,
) where

import UnliftIO.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (MonadIO)

data Ribosome e =
  Ribosome {
    name :: String,
    env :: e
  }

newRibosome :: MonadIO m => String -> e -> m (Ribosome (TVar e))
newRibosome name' env' = do
  tv <- newTVarIO env'
  return $ Ribosome name' tv
