module Proteome.Data.TagsError where

import Exon (exon)
import Log (Severity (Warn))
import Ribosome (Report (Report), Reportable (toReport), SettingError, reportMessages)

data TagsError =
  Process Text
  |
  TempName
  |
  RenameTags Text
  |
  Setting SettingError
  deriving stock (Eq, Show)

instance Reportable TagsError where
  toReport = \case
    Process msg ->
      Report "tags process failed" ["TagsError.Process", msg] Warn
    TempName ->
      Report "failed to create temp dir for tags process" ["TagsError.TempName"] Warn
    RenameTags msg ->
      Report "Failed to rename temporary tags file" ["TagsError.RenameTags", msg] Warn
    Setting e ->
      Report [exon|tags failed: #{reportMessages (toReport e)}|] ["TagsError.Setting:", show e] Warn
