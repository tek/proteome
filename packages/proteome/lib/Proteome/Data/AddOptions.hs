module Proteome.Data.AddOptions where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectType (ProjectType)

data AddOptions =
  AddOptions {
    name :: ProjectName,
    tpe :: ProjectType,
    activate :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
