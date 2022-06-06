module Proteome.Data.ResolveError where

import Exon (exon)
import Log (Severity (Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

newtype ResolveError =
  ParsePath Text
  deriving stock (Eq, Show)

instance ToErrorMessage ResolveError where
  toErrorMessage (ParsePath path) =
    ErrorMessage [exon|invalid path: #{path}|] ["ResolveError.ParsePath:", path] Warn
