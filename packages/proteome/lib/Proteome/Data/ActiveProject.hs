module Proteome.Data.ActiveProject where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectType (ProjectType)

data ActiveProject =
  ActiveProject {
    name :: ProjectName,
    tpe :: ProjectType,
    lang :: Maybe ProjectLang
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
