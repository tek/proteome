module Proteome.Data.ReplaceError where

import Log (Severity (Error, Warn))
import Path (Abs, File, Path)
import Ribosome (Report (Report), Reportable, toReport)
import Ribosome (pathText)

data ReplaceError =
  BadReplacement
  |
  CouldntLoadBuffer (Path Abs File)
  deriving stock (Eq, Show)

instance Reportable ReplaceError where
  toReport BadReplacement =
    Report "replacment line count does not match original" ["ReplaceError.BadReplacement"] Warn
  toReport (CouldntLoadBuffer (pathText -> path)) =
    Report ("could not load file " <> path) ["ReplaceError.CouldntLoadBuffer", path] Error
