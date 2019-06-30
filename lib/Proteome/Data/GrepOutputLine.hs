module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int,
    _text :: Text
  }
  deriving (Eq, Show)

instance Ord GrepOutputLine where
  (GrepOutputLine p1 l1 _ _) <= (GrepOutputLine p2 l2 _ _) =
    (p1, l1) <= (p2, l2)
