module Proteome.Save where

import Control.Monad.Catch (MonadThrow)
import Ribosome.Data.PersistError (PersistError)

import Proteome.Data.Env (Env)
import Proteome.Data.TagsError (TagsError)
import Proteome.PersistBuffers (storeBuffers)
import Proteome.Tags (proTags)
import Ribosome.Data.SettingError (SettingError)

proSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e TagsError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  m ()
proSave = do
  proTags
  storeBuffers
