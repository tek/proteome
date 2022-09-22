module Proteome.Data.Env where

import Ribosome (Buffer)

import Proteome.Data.Project(Project)
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject, VirtualProject))
import Proteome.Data.ProjectType (ProjectType)
import Proteome.Data.Replace (Replace)

data Env =
  Env {
    mainProject :: Project,
    projects :: [Project],
    currentProjectIndex :: Int,
    configLog :: [Text],
    buffers :: [Buffer],
    replace :: Maybe Replace
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

mainType :: Env -> Maybe ProjectType
mainType env =
  case env ^. (#mainProject . #meta) of
    DirProject _ _ tpe ->
      tpe
    VirtualProject _ ->
      Nothing
