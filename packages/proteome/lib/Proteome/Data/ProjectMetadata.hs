module Proteome.Data.ProjectMetadata where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot)
import Proteome.Data.ProjectType (ProjectType)

data ProjectMetadata =
  DirProject {
    name :: ProjectName,
    root :: ProjectRoot,
    tpe :: Maybe ProjectType
  }
  |
  VirtualProject {
    name :: ProjectName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

instance Default ProjectMetadata where
  def =
    VirtualProject (ProjectName "main")
