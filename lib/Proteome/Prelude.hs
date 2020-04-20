{-# LANGUAGE NoImplicitPrelude #-}

module Proteome.Prelude where

import Language.Haskell.TH.Quote (QuasiQuoter)
import NeatInterpolation (trimming)

qt :: QuasiQuoter
qt =
  trimming
{-# INLINE qt #-}
