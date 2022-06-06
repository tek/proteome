module Proteome.Data.ProjectLang where

import Ribosome (MsgpackDecode, MsgpackEncode)

newtype ProjectLang =
  ProjectLang { unProjectLang :: Text }
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (MsgpackEncode, MsgpackDecode, IsString)
