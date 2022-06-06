module Proteome.Data.ProjectConfig where

import Path (Abs, Dir, Path)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectType (ProjectType)

data ProjectConfig =
  ProjectConfig {
    baseDirs :: [Path Abs Dir],
    typeDirs :: Map ProjectType [Path Abs Dir],
    projectTypes :: Map ProjectType [Path Abs Dir],
    typeMap :: Map ProjectType [ProjectType],
    typeMarkers :: Map ProjectType [Text],
    langMap :: Map ProjectType ProjectLang,
    langsMap :: Map ProjectLang [ProjectLang]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode, Default)
