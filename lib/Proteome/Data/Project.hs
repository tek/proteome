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
import Data.Default.Class (Default(def))
import Control.DeepSeq
import Neovim.Classes (NvimObject(..))
import Ribosome.Internal.NvimObject (deriveString)

newtype ProjectName = ProjectName String
  deriving (Eq, Show, Generic, NFData)

instance NvimObject ProjectName where
  toObject (ProjectName s) = toObject s
  fromObject = deriveString ProjectName

newtype ProjectType =
  ProjectType {
    projectType :: String
  }
  deriving (Ord, Eq, Show, Generic, NFData)

instance NvimObject ProjectType where
  toObject (ProjectType s) = toObject s
  fromObject = deriveString ProjectType

newtype ProjectLang = ProjectLang String
  deriving (Eq, Show, Generic, NFData)

instance NvimObject ProjectLang where
  toObject (ProjectLang s) = toObject s
  fromObject = deriveString ProjectLang

data ProjectMetadata =
  DirProject {
    name :: ProjectName,
    root :: FilePath,
    tpe :: Maybe ProjectType
  }
  | VirtualProject {
    name :: ProjectName
  }
  deriving (Eq, Show)

instance Default ProjectMetadata where
  def = VirtualProject (ProjectName "main")

data Project =
  Project {
    meta :: ProjectMetadata,
    types :: [ProjectType],
    lang :: Maybe ProjectLang,
    langs :: [ProjectLang]
  }
  deriving (Eq, Show)

instance Default Project where
  def = Project def def def def
