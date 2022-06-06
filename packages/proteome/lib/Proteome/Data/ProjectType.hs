module Proteome.Data.ProjectType where

import Ribosome (MsgpackDecode, MsgpackEncode)

newtype ProjectType =
  ProjectType { unProjectType :: Text }
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)
