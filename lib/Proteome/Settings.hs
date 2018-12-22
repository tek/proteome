module Proteome.Settings(
  mainProjectDir,
  projects,
  projectTypeDirs,
  projectBaseDirs,
  projectConfig,
  mainName,
  mainType,
  tagsCommand,
  tagsArgs,
  tagsFork,
  tagsFileName,
  active,
) where

import Data.Default.Class (Default(def))
import Ribosome.Config.Setting
import Proteome.Data.Project (ProjectName, ProjectType)
import Proteome.Data.ActiveProject (ActiveProject)
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
projectConfig = Setting "project_config" True (Just (ProjectConfig def def def def))

mainName :: Setting ProjectName
mainName = Setting "main_name" True Nothing

mainType :: Setting ProjectType
mainType = Setting "main_type" True Nothing

active :: Setting ActiveProject
active = Setting "active" True Nothing

tagsCommand :: Setting String
tagsCommand = Setting "tags_command" True (Just "ctags")

tagsArgs :: Setting String
tagsArgs = Setting "tags_args" True (Just "-R --languages={langsComma} -f {tagFile} {root}")

tagsFork :: Setting Bool
tagsFork = Setting "tags_fork" True (Just True)

tagsFileName :: Setting FilePath
tagsFileName = Setting "tags_file_name" True (Just ".tags")
