{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.Env where

import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.Errors (Errors)
import Ribosome.Nvim.Api.Data (Buffer)

import Proteome.Data.Error (Error)
import Proteome.Data.Project(Project)

data Env =
  Env {
    _mainProject :: Project,
    _projects :: [Project],
    _errors :: Errors,
    _currentProjectIndex :: Int,
    _configLog :: [Text],
    _buffers :: [Buffer]
  }
  deriving (Show, Generic, Default)

deepLenses ''Env

type Proteome a = Ribo Env Error a
