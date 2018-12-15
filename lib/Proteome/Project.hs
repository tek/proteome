module Proteome.Project(
  allProjects,
) where

import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (Project)
import Proteome.Data.Env (Env(mainProject, projects))

allProjects :: Proteome [Project]
allProjects = do
  main <- Ribo.inspect mainProject
  extra <- Ribo.inspect projects
  return $ main : extra
