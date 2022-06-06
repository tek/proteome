module Proteome.Data.FilenameError where

import Exon (exon)
import Log (Severity (Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data FilenameError =
  BadCwd
  |
  InvalidPathSpec Text
  |
  BufferPathInvalid
  |
  CreateDir Text
  |
  Exists Text
  |
  ActionFailed Text Text
  |
  Remove Text
  deriving stock (Eq, Show, Generic)

instance ToErrorMessage FilenameError where
  toErrorMessage = \case
    BadCwd ->
      ErrorMessage "Could not determine current directory" ["FilenameError.BadCwd"] Error
    InvalidPathSpec spec ->
      ErrorMessage [exon|Invalid path: #{spec}|] ["FilenameError.InvalidPathSpec:", spec] Warn
    BufferPathInvalid ->
      ErrorMessage "Current buffer is not an existing file" ["FilenameError.BufferPathInvalid"] Warn
    CreateDir dir ->
      ErrorMessage [exon|Could not create directory #{dir}|] ["FilenameError.CreateDir:", dir] Warn
    Exists file ->
      ErrorMessage [exon|File already exists: #{file}|] ["FilenameError.Exists:", file] Warn
    ActionFailed action err ->
      ErrorMessage "File system error" ["FilenameError.ActionFailed:", action, err] Warn
    Remove err ->
      ErrorMessage "Couldn't remove the source file" ["FilenameError.Remove:", err] Warn
