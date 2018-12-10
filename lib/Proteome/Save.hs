module Proteome.Save(
  proSave,
) where

import Proteome.Data.Proteome (Proteome)
import Proteome.Tags (proTags)

proSave :: Proteome ()
proSave = proTags
