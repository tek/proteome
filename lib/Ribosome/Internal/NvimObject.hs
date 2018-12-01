module Ribosome.Internal.NvimObject(
  deriveString,
) where

import Neovim (Object, Doc, AnsiStyle, fromObject)

deriveString :: (String -> a) -> Object -> Either (Doc AnsiStyle) a
deriveString cons o = fmap cons ((fromObject o) :: Either (Doc AnsiStyle) String)
