module Proteome.Data.ReplaceError where

import Log (Severity (Error, Warn))
import Path (Abs, File, Path)
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage, toErrorMessage)
import Ribosome (pathText)

data ReplaceError =
  BadReplacement
  |
  CouldntLoadBuffer (Path Abs File)
  deriving stock (Eq, Show)

instance ToErrorMessage ReplaceError where
  toErrorMessage BadReplacement =
    ErrorMessage "replacment line count does not match original" ["ReplaceError.BadReplacement"] Warn
  toErrorMessage (CouldntLoadBuffer (pathText -> path)) =
    ErrorMessage ("could not load file " <> path) ["ReplaceError.CouldntLoadBuffer", path] Error
