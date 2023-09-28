module Proteome.Data.ReplaceError where

import Exon (exon)
import Log (Severity (Error))
import Path (Abs, File, Path)
import Ribosome (Report (Report), Reportable, pathText, toReport)

data ReplaceError =
  BadReplacement
  |
  BufferErrors (NonEmpty (Path Abs File, Text))
  deriving stock (Eq, Show)

instance Reportable ReplaceError where
  toReport BadReplacement =
    Report "replacment line count does not match original" ["ReplaceError.BadReplacement"] Error
  toReport (BufferErrors (fmap (first pathText) -> paths)) =
    Report user ("ReplaceError.BufferErrors" : (toList paths >>= \ (p, e) -> [p, e])) Error
    where
      user | [(path, _)] <- paths = [exon|Writing the buffer failed for: #{path}|]
           | otherwise = "Writing the buffer failed for several files"
