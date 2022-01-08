module Proteome.Data.ProjectRoot where

import Path (Abs, Dir, Path)

newtype ProjectRoot =
  ProjectRoot { unProjectRoot :: Path Abs Dir }
  deriving stock (Eq, Show, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode)
