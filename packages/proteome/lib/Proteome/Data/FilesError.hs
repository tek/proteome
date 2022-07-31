module Proteome.Data.FilesError where

import Log (Severity (Error, Warn))
import Ribosome (Report (Report), Reportable (toReport))

data FilesError =
  BadCwd
  |
  NoSuchPath Text
  |
  BadRegex Text Text
  |
  InvalidFilePath Text
  |
  CouldntCreateDir Text
  deriving stock (Eq, Show)

instance Reportable FilesError where
  toReport BadCwd =
    Report "internal error" ["FilesError.BadCwd"] Error
  toReport (NoSuchPath path) =
    Report ("path doesn't exist: " <> path) ["FilesError.NoSuchPath:", path] Warn
  toReport (BadRegex var re) =
    Report ("bad regex in `g:proteome_" <> var <> "`: " <> re) ["FilesError.BadRegex:", var, re] Warn
  toReport (InvalidFilePath path) =
    Report ("invalid file path: " <> path) ["FilesError.InvalidFilePath:", path] Warn
  toReport (CouldntCreateDir path) =
    Report ("couldn't create directory: " <> path) ["FilesError.CouldntCreateDir:", path] Warn
