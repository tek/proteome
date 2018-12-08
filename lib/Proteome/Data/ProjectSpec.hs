{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Proteome.Data.ProjectSpec(
  ProjectSpec (..),
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.MessagePack
import Neovim.Classes (NvimObject(..), Dictionary)
import Proteome.Data.Project (ProjectName, ProjectType, ProjectLang, ProjectRoot)

data ProjectSpec =
  ProjectSpec {
    name :: ProjectName,
    root :: ProjectRoot,
    tpe :: Maybe ProjectType,
    types :: [ProjectType],
    lang :: Maybe ProjectLang,
    langs :: [ProjectLang]
  }
  deriving (Generic, NFData)

instance NvimObject ProjectSpec where
  toObject ProjectSpec {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("name", toObject name),
      ("root", toObject root),
      ("types", toObject types),
      ("langs", toObject langs)
    ] ++ catMaybes
    [
      tpe >>= \a -> return ("tpe", toObject a),
      lang >>= \a -> return ("lang", toObject a)
    ]
