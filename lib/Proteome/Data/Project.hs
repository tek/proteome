{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.Project(
  ProjectMetadata (..),
  Project (..),
  ProjectType (..),
  ProjectLang (..),
  ProjectRoot (..),
  ProjectName (..),
  _meta,
  _lang,
  langOrType,
) where

import GHC.Generics (Generic)
import Control.Lens (makeClassy_)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default(def))
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

newtype ProjectLang =
  ProjectLang
  {
    projectLang :: String
  }
  deriving (Eq, Show, Generic, NFData)

instance NvimObject ProjectLang where
  toObject (ProjectLang s) = toObject s
  fromObject = deriveString ProjectLang

newtype ProjectRoot = ProjectRoot FilePath
  deriving (Eq, Show, Generic, NFData)

instance NvimObject ProjectRoot where
  toObject (ProjectRoot s) = toObject s
  fromObject = deriveString ProjectRoot

data ProjectMetadata =
  DirProject {
    name :: ProjectName,
    root :: ProjectRoot,
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

makeClassy_ ''Project

instance Default Project where
  def = Project def def def def

langOrType :: Maybe ProjectLang -> Maybe ProjectType -> Maybe ProjectLang
langOrType (Just lang') _ = Just lang'
langOrType Nothing (Just (ProjectType tpe')) = Just (ProjectLang tpe')
langOrType _ _ = Nothing
