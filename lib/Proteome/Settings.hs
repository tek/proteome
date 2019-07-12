{-# LANGUAGE QuasiQuotes #-}

module Proteome.Settings where

import Path (Abs, Dir, File, Path, Rel, relfile)

import Proteome.Data.ActiveProject (ActiveProject)
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectName (ProjectName)
import Proteome.Data.ProjectSpec (ProjectSpec)
import Proteome.Data.ProjectType (ProjectType)
import Ribosome.Data.Setting (Setting(Setting))

mainProjectDir :: Setting (Path Abs Dir)
mainProjectDir = Setting "main_project_dir" True Nothing

projects :: Setting [ProjectSpec]
projects = Setting "projects" True (Just [])

projectConfig :: Setting ProjectConfig
projectConfig = Setting "project_config" True (Just (ProjectConfig def def def def def def def))

mainName :: Setting ProjectName
mainName = Setting "main_name" True Nothing

mainType :: Setting ProjectType
mainType = Setting "main_type" True Nothing

active :: Setting ActiveProject
active = Setting "active" True Nothing

tagsCommand :: Setting Text
tagsCommand = Setting "tags_command" True (Just "ctags")

tagsArgs :: Setting Text
tagsArgs = Setting "tags_args" True (Just "-R --languages={langsComma} -f {tagFile} {root}")

tagsFork :: Setting Bool
tagsFork = Setting "tags_fork" True (Just True)

tagsFileName :: Setting (Path Rel File)
tagsFileName = Setting "tags_file_name" True (Just [relfile|.tags|])

grepCmdline :: Setting Text
grepCmdline =
  Setting "grep_cmdline" True (Just "grep -Hnr ${pattern} ${path}")

filesCmdline :: Setting Text
filesCmdline =
  Setting "files_cmdline" True (Just "rg --files ${paths}")
