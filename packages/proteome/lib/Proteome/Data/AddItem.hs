module Proteome.Data.AddItem where

data AddItem =
  AddItem {
     projectType :: Text,
     projectName :: Text
  }
  deriving stock (Eq, Show, Ord)
