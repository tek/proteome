module Proteome.Data.ProjectType where

import Data.String (IsString(..))

newtype ProjectType =
  ProjectType { _tpe :: Text }
  deriving (Ord, Eq, Show, Generic, MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectType
