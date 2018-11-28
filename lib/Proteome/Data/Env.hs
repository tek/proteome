module Proteome.Data.Env (
  Env(..)
) where

import Proteome.Data.Project

data Env = Env {
  mainProject :: Project,
  projects :: [Project]
}
