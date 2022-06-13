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
  EmptyUserInput Text
  deriving stock (Eq, Show)

instance ToErrorMessage GrepError where
  toErrorMessage = \case
    Empty ->
      ErrorMessage "grep cmdline is empty" ["GrepError.Empty"] Warn
    NotInPath exe ->
      ErrorMessage ("grep executable `" <> exe <> "` not found in $PATH") ["GrepError.NotInPath:", exe] Warn
    NoSuchExecutable exe ->
      ErrorMessage ("grep executable `" <> exe <> "` does not exist") ["GrepError.NoSuchExecutable:", exe] Warn
    NoSuchDestination (pathText -> dir) ->
      ErrorMessage [exon|grep destination `#{dir}` does not exist|] ["GrepError.NoSuchDestination:", dir] Warn
    EmptyUserInput what ->
      ErrorMessage [exon|no #{what} given|] ["GrepError.EmptyPattern"] Warn
