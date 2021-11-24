module Proteome.Data.ProjectName where

newtype ProjectName = ProjectName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode, IsString)
