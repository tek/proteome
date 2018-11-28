module Ribosome.Data.Ribosome(
  Ribosome (..)
) where

data Ribosome e =
  Ribosome {
    name :: String,
    env :: e
  }
