module Proteome.Data.ProjectName where

import Ribosome (MsgpackDecode, MsgpackEncode)

newtype ProjectName =
  ProjectName { unProjectName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode, IsString)
