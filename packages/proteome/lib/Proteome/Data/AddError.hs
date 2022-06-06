module Proteome.Data.AddError where

import Exon (exon)
import Log (Severity (Info))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data AddError =
  InvalidProjectSpec Text
  |
  Directory Text
  deriving stock (Eq, Show)

instance ToErrorMessage AddError where
  toErrorMessage = \case
    InvalidProjectSpec spec ->
      ErrorMessage [exon|no such project: #{spec}|] ["AddError.InvalidProjectSpec:", spec] Info
    Directory msg ->
      ErrorMessage [exon|Listing directories: #{msg}|] ["AddError.Directory:", msg] Info
