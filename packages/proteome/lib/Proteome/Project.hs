module Proteome.Project where

import Lens.Micro.Extras (preview)
import Path (Abs, Dir, Path, dirname, parent)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (currentProjectIndex, mainProject, projects)
import Proteome.Data.Project (Project)
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Path (dropSlash)

allProjects ::
  Member (AtomicState Env) r =>
  Sem r [Project]
allProjects = do
  main <- atomicGets Env.mainProject
  extra <- atomicGets Env.projects
  pure $ main : extra

currentProject ::
  Member (AtomicState Env) r =>
  Sem r (Maybe Project)
currentProject = do
  index <- atomicGets Env.currentProjectIndex
  preview (ix index) <$> allProjects

pathData :: Path Abs Dir -> (ProjectRoot, ProjectName, ProjectType)
pathData root =
  (
    ProjectRoot root,
    ProjectName . dropSlash . dirname $ root,
    ProjectType . dropSlash . dirname . parent $ root
  )
