module Proteome.Data.FilenameError where

import Exon (exon)
import Log (Severity (Error, Warn))
import Ribosome (Report (Report), Reportable (toReport))

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

instance Reportable FilenameError where
  toReport = \case
    BadCwd ->
      Report "Could not determine current directory" ["FilenameError.BadCwd"] Error
    InvalidPathSpec spec ->
      Report [exon|Invalid path: #{spec}|] ["FilenameError.InvalidPathSpec:", spec] Warn
    BufferPathInvalid ->
      Report "Current buffer is not an existing file" ["FilenameError.BufferPathInvalid"] Warn
    CreateDir dir ->
      Report [exon|Could not create directory #{dir}|] ["FilenameError.CreateDir:", dir] Warn
    Exists file ->
      Report [exon|File already exists: #{file}|] ["FilenameError.Exists:", file] Warn
    ActionFailed action err ->
      Report "File system error" ["FilenameError.ActionFailed:", action, err] Warn
    Remove err ->
      Report "Couldn't remove the source file" ["FilenameError.Remove:", err] Warn
