{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.FilesConfig where

import Text.RE.PCRE.Text (RE)

data FilesConfig =
  FilesConfig {
    _ignoreHidden :: Bool,
    _ignoreFiles :: [RE],
    _ignoreDirs :: [RE]
  }

makeClassy ''FilesConfig
