{-# LANGUAGE TemplateHaskell #-}

module Proteome.Data.Env (
  Env(..),
  _mainProject,
  _projects,
  _errors,
  _currentProjectIndex,
  _configLog,
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Ribosome.Data.Errors (Errors)
import Proteome.Data.Project(Project)

data Env =
  Env {
    mainProject :: Project,
    projects :: [Project],
    errors :: Errors,
    currentProjectIndex :: Int,
    configLog :: [FilePath]
  }
  deriving Show

makeClassy_ ''Env

instance Default Env where
  def = Env def def def def def
