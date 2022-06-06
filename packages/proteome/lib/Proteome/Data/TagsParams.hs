module Proteome.Data.TagsParams where

import Proteome.Data.ProjectLang (ProjectLang)
import Proteome.Data.ProjectRoot (ProjectRoot)

data TagsParams =
  TagsParams {
    root :: ProjectRoot,
    langs :: [ProjectLang]
  }
  deriving stock (Eq, Show)
