{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.Error where

import Ribosome.Data.RiboError (RiboError)
import Ribosome.Error.Report.Class (ReportError(..))

import Proteome.Data.AddError (AddError)
import Proteome.Data.GrepError (GrepError)
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Data.TagsError (TagsError)

data Error =
  Rib RiboError
  |
  Add AddError
  |
  Tags TagsError
  |
  Grep GrepError
  |
  Resolve ResolveError
  deriving (Show, Generic, ReportError)

deepPrisms ''Error
