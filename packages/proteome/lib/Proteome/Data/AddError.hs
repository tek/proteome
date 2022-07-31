module Proteome.Data.AddError where

import Exon (exon)
import Log (Severity (Info))
import Ribosome (Report (Report), Reportable (toReport))

data AddError =
  InvalidProjectSpec Text
  |
  Directory Text
  deriving stock (Eq, Show)

instance Reportable AddError where
  toReport = \case
    InvalidProjectSpec spec ->
      Report [exon|no such project: #{spec}|] ["AddError.InvalidProjectSpec:", spec] Info
    Directory msg ->
      Report [exon|Listing directories: #{msg}|] ["AddError.Directory:", msg] Info
