module Proteome.Data.Env  where

import Proteome.Data.Project(Project)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Errors (Errors)

import Proteome.Data.Error (Error)

data Env =
  Env {
    _mainProject :: Project,
    _projects :: [Project],
    _errors :: Errors,
    _currentProjectIndex :: Int,
    _configLog :: [Text]
  }
  deriving Show

deepLenses ''Env

instance Default Env where
  def = Env def def def def def

type Proteome a = Ribo Env Error a
