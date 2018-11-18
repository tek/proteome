module Proteome.Config(
  proReadConfig
)
where

import Neovim

proReadConfig :: Neovim env String
proReadConfig = return "foo"
