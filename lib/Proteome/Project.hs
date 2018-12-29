module Proteome.Project(
  allProjects,
  currentProject,
  pathData,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Safe (atMay)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath (takeFileName, takeDirectory)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project,
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  )
import Proteome.Data.Env (Env(mainProject, projects, currentProjectIndex))

allProjects :: Proteome [Project]
allProjects = do
  main <- Ribo.inspect mainProject
  extra <- Ribo.inspect projects
  return $ main : extra

currentProject :: Proteome (Maybe Project)
currentProject = do
  index <- Ribo.inspect currentProjectIndex
  pros <- allProjects
  return $ atMay pros index

pathData :: MonadIO m => Maybe FilePath -> m (ProjectRoot, ProjectName, ProjectType)
pathData override = do
  cwd <- liftIO $ maybe getCurrentDirectory pure override
  absMainDir <- liftIO $ makeAbsolute cwd
  return (
    ProjectRoot absMainDir,
    ProjectName $ takeFileName absMainDir,
    ProjectType $ (takeFileName . takeDirectory) absMainDir
    )
