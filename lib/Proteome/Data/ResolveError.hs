{-# LANGUAGE QuasiQuotes #-}

module Proteome.Data.ResolveError where

import Data.String.QM (qt)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log.Logger (Priority(NOTICE))

newtype ResolveError =
  ParsePath Text
  deriving (Eq, Show)

deepPrisms ''ResolveError

instance ReportError ResolveError where
  errorReport (ParsePath path) =
    ErrorReport [qt|invalid path: ${path} |] ["ResolveError.ParsePath:", path] NOTICE
