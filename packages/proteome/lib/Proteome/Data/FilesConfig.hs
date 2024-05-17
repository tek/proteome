module Proteome.Data.FilesConfig where

import Text.Regex.PCRE.Light (Regex)

data FilesConfig =
  FilesConfig {
    useRg :: Bool,
    rgExclude :: [Text],
    ignoreHidden :: Bool,
    ignoreFiles :: [Regex],
    ignoreDirs :: [Regex],
    wildignore :: [Text]
  }
  deriving stock (Eq, Show, Generic)
