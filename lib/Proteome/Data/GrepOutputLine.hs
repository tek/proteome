module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int,
    _text :: Text
  }
  deriving (Eq, Show)

newtype PerLine =
  PerLine { unPerLine :: GrepOutputLine }
  deriving (Show)

instance Eq PerLine where
  PerLine (GrepOutputLine p1 l1 _ _) == PerLine (GrepOutputLine p2 l2 _ _) =
    p1 == p2 && l1 == l2

instance Ord PerLine where
  (PerLine (GrepOutputLine p1 l1 _ _)) <= (PerLine (GrepOutputLine p2 l2 _ _)) =
    (p1, l1) <= (p2, l2)
