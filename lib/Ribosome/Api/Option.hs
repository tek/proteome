module Ribosome.Api.Option(
  optionCat,
  rtpCat
) where

import Neovim

optionCat :: String -> String -> Neovim env ()
optionCat name extra = do
  current <- vim_get_option' name >>= fromObject'
  vim_set_option' name $ toObject $ current ++ "," ++ extra

rtpCat :: String -> Neovim env ()
rtpCat = optionCat "runtimepath"
