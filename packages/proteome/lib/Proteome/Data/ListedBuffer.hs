module Proteome.Data.ListedBuffer where

import Ribosome.Host.Api.Data (Buffer)

data ListedBuffer =
  ListedBuffer {
    buffer :: Buffer,
    number :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)
