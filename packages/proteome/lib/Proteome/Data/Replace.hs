module Proteome.Data.Replace where

import Prelude hiding (lines)
import Ribosome (ScratchState)

import Proteome.Data.GrepState (GrepOutputLine)

data Replace =
  Replace {
    scratch :: ScratchState,
    lines :: NonEmpty GrepOutputLine
  }
  deriving stock (Eq, Show, Generic)
