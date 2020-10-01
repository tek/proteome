module Proteome.Data.ProjectType where

newtype ProjectType =
  ProjectType { _tpe :: Text }
  deriving (Ord, Eq, Show, Generic, MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectType
