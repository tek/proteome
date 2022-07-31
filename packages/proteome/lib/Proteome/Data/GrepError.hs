module Proteome.Data.GrepError where

import Exon (exon)
import Log (Severity (Warn))
import Path (Abs, Dir, Path)
import Ribosome (Report (Report), Reportable (toReport), pathText)

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

instance Reportable GrepError where
  toReport = \case
    Empty ->
      Report "grep cmdline is empty" ["GrepError.Empty"] Warn
    NotInPath exe ->
      Report ("grep executable `" <> exe <> "` not found in $PATH") ["GrepError.NotInPath:", exe] Warn
    NoSuchExecutable exe ->
      Report ("grep executable `" <> exe <> "` does not exist") ["GrepError.NoSuchExecutable:", exe] Warn
    NoSuchDestination (pathText -> dir) ->
      Report [exon|grep destination `#{dir}` does not exist|] ["GrepError.NoSuchDestination:", dir] Warn
    EmptyUserInput what ->
      Report [exon|no #{what} given|] ["GrepError.EmptyPattern"] Warn
