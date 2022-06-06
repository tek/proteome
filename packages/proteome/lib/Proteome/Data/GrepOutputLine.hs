module Proteome.Data.GrepOutputLine where

import Path (Abs, File, Path)
import Ribosome (MsgpackEncode)

data GrepOutputLine =
  GrepOutputLine {
    path :: Path Abs File,
    line :: Int,
    col :: Maybe Int,
    content :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode)

sameLine :: GrepOutputLine -> GrepOutputLine -> Bool
sameLine (GrepOutputLine p1 l1 _ _) (GrepOutputLine p2 l2 _ _) =
  p1 == p2 && l1 == l2
