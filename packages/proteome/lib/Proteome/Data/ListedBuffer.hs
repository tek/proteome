module Proteome.Data.ListedBuffer where

import Ribosome.Nvim.Api.Data (Buffer)

data ListedBuffer =
  ListedBuffer {
    _buffer :: Buffer,
    _number :: Int,
    _name :: Text
  }
  deriving stock (Eq, Show)

makeClassy 'ListedBuffer
