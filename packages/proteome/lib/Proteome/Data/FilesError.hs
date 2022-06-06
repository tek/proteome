module Proteome.Data.FilesError where

import Log (Severity (Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

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

instance ToErrorMessage FilesError where
  toErrorMessage BadCwd =
    ErrorMessage "internal error" ["FilesError.BadCwd"] Error
  toErrorMessage (NoSuchPath path) =
    ErrorMessage ("path doesn't exist: " <> path) ["FilesError.NoSuchPath:", path] Warn
  toErrorMessage (BadRegex var re) =
    ErrorMessage ("bad regex in `g:proteome_" <> var <> "`: " <> re) ["FilesError.BadRegex:", var, re] Warn
  toErrorMessage (InvalidFilePath path) =
    ErrorMessage ("invalid file path: " <> path) ["FilesError.InvalidFilePath:", path] Warn
  toErrorMessage (CouldntCreateDir path) =
    ErrorMessage ("couldn't create directory: " <> path) ["FilesError.CouldntCreateDir:", path] Warn
