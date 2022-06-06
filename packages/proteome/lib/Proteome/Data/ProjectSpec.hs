module Proteome.Data.ProjectSpec where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectRoot (ProjectRoot)
import Proteome.Data.ProjectType (ProjectType)

data ProjectSpec =
  ProjectSpec {
    name :: ProjectName,
    root :: ProjectRoot,
    tpe :: Maybe ProjectType,
    types :: [ProjectType],
    lang :: Maybe ProjectLang,
    langs :: [ProjectLang]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
