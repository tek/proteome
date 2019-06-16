module Proteome.Quit where

import Control.Monad.Catch (MonadThrow)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)

import Proteome.Data.Env (Env)
import Proteome.PersistBuffers (storeBuffers)

proQuit ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  m ()
proQuit =
  storeBuffers
