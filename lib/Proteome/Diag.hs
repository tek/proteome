module Proteome.Diag(
  proDiag,
) where

import Data.Functor (void)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)
import Proteome.Data.Proteome (Proteome)

proDiag :: CommandArguments -> Proteome ()
proDiag _ =
  void $ showInScratch ["Diagnostics"] (defaultScratchOptions "proteome-diagnostics")
