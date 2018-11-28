module Ribosome.Internal.IO(
  retypeNeovim,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT, withReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Neovim.Context.Internal (Neovim(..), Config(..), retypeConfig)

-- try using Contravariant with this
retypeNeovim :: (e0 -> e1) -> Neovim e1 a -> Neovim e0 a
retypeNeovim transform thunk = do
  env <- Neovim ask
  liftIO $ runReaderT (withReaderT (newEnv env) $ runResourceT $ unNeovim thunk) env
  where
    newEnv = retypeConfig . transform . customConfig
