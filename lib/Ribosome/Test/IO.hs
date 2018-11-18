module Ribosome.Test.IO(
  nvimFail
)
where

import Neovim

nvimFail :: Either NeovimException a -> Neovim env a
nvimFail (Right a) = return a
nvimFail (Left e) = (liftIO . fail . show) e
