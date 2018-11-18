module Proteome.Add(
  proAdd,
  proteomePoll
)
where

import Neovim

proAdd :: Neovim env String
proAdd = return "foo"

proteomePoll :: Neovim env Bool
proteomePoll = return True
