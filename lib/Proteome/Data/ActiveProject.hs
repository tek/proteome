{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Proteome.Data.ActiveProject(
  ActiveProject(ActiveProject),
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map as Map (fromList)
import Neovim (Object)
import Neovim.Classes (NvimObject(..), Dictionary)
import Proteome.Data.Project(
  ProjectName,
  ProjectType
  )

data ActiveProject =
  ActiveProject {
    name :: ProjectName,
    tpe :: ProjectType
  }
  deriving (Eq, Show, Generic, NFData)

instance NvimObject ActiveProject where
  toObject ActiveProject {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("name", toObject name),
      ("tpe", toObject tpe)
    ]
  fromObject _ = Left "ActiveProject is write-only"
