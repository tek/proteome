module Proteome.Save(
  proSave,
) where

import Proteome.Data.Proteome (Proteome)
import Proteome.Tags (proTags)
import Proteome.PersistBuffers (storeBuffers)

proSave :: Proteome ()
proSave = do
  proTags
  storeBuffers
