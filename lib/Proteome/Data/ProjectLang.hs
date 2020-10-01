module Proteome.Data.ProjectLang where

newtype ProjectLang =
  ProjectLang { _lang :: Text }
  deriving (Ord, Eq, Show, Generic, MsgpackDecode, MsgpackEncode, IsString)

makeClassy ''ProjectLang
