module Ribosome.Data.Ribo(
  Ribo
) where

import Neovim (Neovim)
import Ribosome.Data.Ribosome

type Ribo e = Neovim (Ribosome e)
