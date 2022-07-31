module Proteome.Data.ResolveError where

import Exon (exon)
import Log (Severity (Warn))
import Ribosome (Report (Report), Reportable (toReport))

newtype ResolveError =
  ParsePath Text
  deriving stock (Eq, Show)

instance Reportable ResolveError where
  toReport (ParsePath path) =
    Report [exon|invalid path: #{path}|] ["ResolveError.ParsePath:", path] Warn
