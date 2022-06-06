module Proteome.Data.ProjectRoot where

import Path (Abs, Dir, Path)
import Ribosome (MsgpackDecode, MsgpackEncode)

newtype ProjectRoot =
  ProjectRoot { unProjectRoot :: Path Abs Dir }
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode)
