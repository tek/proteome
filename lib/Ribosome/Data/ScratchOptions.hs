module Ribosome.Data.ScratchOptions(
  ScratchOptions(..),
) where

data ScratchOptions =
  ScratchOptions {
    tab :: Bool,
    vertical :: Bool,
    size :: Maybe Int,
    wrap :: Bool,
    name :: String
  }
