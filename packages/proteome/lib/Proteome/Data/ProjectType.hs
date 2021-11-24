module Proteome.Data.ProjectType where

newtype ProjectType =
  ProjectType { _tpe :: Text }
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectType
