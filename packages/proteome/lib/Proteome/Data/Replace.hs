module Proteome.Data.Replace where

import Prelude hiding (lines)
import Ribosome (ScratchState)

import Proteome.Data.GrepOutputLine (GrepOutputLine)

data Replace =
  Replace {
    scratch :: ScratchState,
    lines :: NonEmpty GrepOutputLine
  }
  deriving stock (Eq, Show, Generic)
