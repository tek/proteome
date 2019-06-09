module Proteome.Data.ProjectLang where

import Data.String (IsString(..))

newtype ProjectLang =
  ProjectLang { _lang :: Text }
  deriving (Ord, Eq, Show, Generic, MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectLang
