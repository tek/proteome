module Proteome.Settings(
  mainProjectDir,
  projects,
  projectTypeDirs,
  projectBaseDirs,
  projectConfig,
  mainName,
  mainType,
) where

import qualified Data.Map as Map
import Ribosome.Config.Settings
import Proteome.Data.Project (ProjectName, ProjectType)
import Proteome.Data.ProjectSpec (ProjectSpec)
import Proteome.Config

mainProjectDir :: Setting String
mainProjectDir = Setting "main_project_dir" True Nothing

projects :: Setting [ProjectSpec]
projects = Setting "projects" True (Just [])

projectBaseDirs :: Setting [FilePath]
projectBaseDirs = Setting "project_base_dirs" True (Just [])

projectTypeDirs :: Setting [FilePath]
projectTypeDirs = Setting "project_type_dirs" True (Just [])

projectConfig :: Setting ProjectConfig
projectConfig = Setting "project_config" True (Just (ProjectConfig Map.empty))

mainName :: Setting ProjectName
mainName = Setting "main_name" True Nothing

mainType :: Setting ProjectType
mainType = Setting "main_type" True Nothing
