module Proteome.Data.ProjectName where

import Data.String (IsString(..))

newtype ProjectName = ProjectName Text
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode, IsString)
