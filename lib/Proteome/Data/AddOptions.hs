{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Proteome.Data.AddOptions(
  AddOptions(..),
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map as Map (fromList)
import Data.Text.Prettyprint.Doc ((<+>), viaShow)
import Neovim (NvimObject(..), Dictionary, Object(ObjectMap))
import Ribosome.Internal.NvimObject (extractObject, extractObjectOr)
import Proteome.Data.Project (ProjectName, ProjectType)

data AddOptions =
  AddOptions {
    name :: ProjectName,
    tpe :: ProjectType,
    activate :: Bool
  }
  deriving (Eq, Show, Generic, NFData)

instance NvimObject AddOptions where
  toObject AddOptions {..} =
    (toObject :: Dictionary -> Object) . Map.fromList $
    [
      ("name", toObject name),
      ("tpe", toObject tpe),
      ("activate", toObject activate)
    ]
  fromObject (ObjectMap o) = do
    name' <- extractObject "name" o
    tpe' <- extractObject "tpe" o
    return $ AddOptions name' tpe' (extractObjectOr "activate" False o)
  fromObject o = Left ("invalid type for AddOptions: " <+> viaShow o)
