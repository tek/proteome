{-# LANGUAGE TemplateHaskell #-}

module Proteome.Data.Env (
  Env(..),
  _mainProject,
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Proteome.Data.Project

data Env = Env {
  mainProject :: Project,
  projects :: [Project]
}

makeClassy_ ''Env

instance Default Env where
  def = Env def def
