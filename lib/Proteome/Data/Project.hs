module Proteome.Data.Project(
  ProjectMetadata(..),
  Project(..)
) where

data ProjectMetadata =
  DirProject {
    name :: String,
    root :: FilePath,
    tpe :: Maybe String
  }
  | VirtualProject {
    name :: String
  }

data Project = Project {
  meta :: ProjectMetadata,
  types :: [String],
  lang :: Maybe String,
  langs :: [String]
}
