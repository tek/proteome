module Proteome.Data.PersistBuffers where

import Path (Abs, File, Path)

data PersistBuffers =
  PersistBuffers {
    current :: Maybe (Path Abs File),
    buffers :: [Path Abs File]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
