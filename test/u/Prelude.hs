{-# LANGUAGE NoImplicitPrelude #-}

module Prelude (
  module Ribosome.Test.PreludeExport,
  module Proteome.Prelude,
) where

import Proteome.Prelude
import Ribosome.Test.PreludeExport hiding (tmuxSpec)
