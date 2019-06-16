module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine Text Int (Maybe Int) Text
  deriving (Eq, Show)
