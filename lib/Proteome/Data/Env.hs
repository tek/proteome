module Proteome.Data.Env (
  Env(..)
) where

import Proteome.Data.Project

data Env = Env {
  main :: (Maybe Project),
  projects :: [Project]
}
