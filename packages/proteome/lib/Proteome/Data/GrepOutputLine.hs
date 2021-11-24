module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int,
    _content :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode)

makeClassy ''GrepOutputLine

sameLine :: GrepOutputLine -> GrepOutputLine -> Bool
sameLine (GrepOutputLine p1 l1 _ _) (GrepOutputLine p2 l2 _ _) =
  p1 == p2 && l1 == l2
