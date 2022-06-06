module Proteome.Data.GrepError where

import Exon (exon)
import Log (Severity (Warn))
import Path (Abs, Dir, Path)
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage), pathText)

data GrepError =
  Empty
  |
  NotInPath Text
  |
  NoSuchExecutable Text
  |
  NoSuchDestination (Path Abs Dir)
  |
  EmptyPattern
  deriving stock (Eq, Show)

instance ToErrorMessage GrepError where
  toErrorMessage Empty =
    ErrorMessage "grep cmdline is empty" ["GrepError.Empty"] Warn
  toErrorMessage (NotInPath exe) =
    ErrorMessage ("grep executable `" <> exe <> "` not found in $PATH") ["GrepError.NotInPath:", exe] Warn
  toErrorMessage (NoSuchExecutable exe) =
    ErrorMessage ("grep executable `" <> exe <> "` does not exist") ["GrepError.NoSuchExecutable:", exe] Warn
  toErrorMessage (NoSuchDestination (pathText -> dir)) =
    ErrorMessage [exon|grep destination `#{dir}` does not exist|] ["GrepError.NoSuchDestination:", dir] Warn
  toErrorMessage EmptyPattern =
    ErrorMessage "no pattern given" ["GrepError.EmptyPattern"] Warn
