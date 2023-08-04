module Proteome.Data.Replace where

import Path (File, SomeBase)
import Prelude hiding (lines)
import Ribosome (ScratchState)

import Proteome.Data.GrepState (GrepOutputLine)

data Replace =
  Replace {
    scratch :: ScratchState,
    lines :: Map (SomeBase File) [GrepOutputLine]
  }
  deriving stock (Eq, Show, Generic)
