{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.Project(
  ProjectMetadata (..),
  Project (..),
  ProjectType (..),
  ProjectLang (..),
  ProjectName (..),
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Neovim.Classes (NvimObject(..))

newtype ProjectName = ProjectName String
  deriving (Eq, Generic, NFData)

instance NvimObject ProjectName where
  toObject (ProjectName s) = toObject s

newtype ProjectType =
  ProjectType {
    projectType :: String
  }
  deriving (Ord, Eq, Generic, NFData)

instance NvimObject ProjectType where
  toObject (ProjectType s) = toObject s

newtype ProjectLang = ProjectLang String
  deriving (Eq, Generic, NFData)

instance NvimObject ProjectLang where
  toObject (ProjectLang s) = toObject s

data ProjectMetadata =
  DirProject {
    name :: ProjectName,
    root :: FilePath,
    tpe :: Maybe ProjectType
  }
  | VirtualProject {
    name :: ProjectName
  }

data Project =
  Project {
    meta :: ProjectMetadata,
    types :: [ProjectType],
    lang :: Maybe ProjectLang,
    langs :: [ProjectLang]
  }
