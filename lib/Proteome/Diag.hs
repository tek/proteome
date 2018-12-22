module Proteome.Diag(
  proDiag,
) where

import Data.Functor (void)
import Data.List (intercalate)
import Data.Map (foldMapWithKey)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Data.Errors (Error(Error), Errors(Errors), ComponentName(ComponentName))
import Ribosome.Scratch (showInScratch)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Proteome.Env (getMainProject)
import qualified Proteome.Data.Env as Env (configLog, projects, errors)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project (Project),
  ProjectLang(..),
  ProjectRoot(ProjectRoot),
  ProjectName(ProjectName),
  ProjectType(..),
  ProjectMetadata (DirProject, VirtualProject),
  )
import Proteome.Tags (tagsCommand)

formatLang :: Maybe ProjectLang -> String
formatLang (Just (ProjectLang lang)) = lang
formatLang Nothing = "none"

formatType :: Maybe ProjectType -> String
formatType (Just (ProjectType tpe)) = tpe
formatType Nothing = "none"

formatMeta :: ProjectMetadata -> [ProjectLang] -> Proteome [String]
formatMeta (VirtualProject (ProjectName name)) _ = return ["name: " ++ name]
formatMeta (DirProject (ProjectName name) r@(ProjectRoot root) tpe) langs = do
  (tagsCmd, tagsArgs) <- tagsCommand r langs
  return [
    "name: " ++ name,
    "root: " ++ root,
    "type: " ++ formatType tpe,
    "tags cmd: " ++ tagsCmd ++ " " ++ tagsArgs
    ]

formatMain :: Project -> Proteome [String]
formatMain (Project meta types lang langs) = do
  metaContent <- formatMeta meta langs
  return $ metaContent ++ [
    "types: " ++ intercalate ", " (fmap projectType types),
    "main language: " ++ formatLang lang,
    "languages: " ++ intercalate ", " (fmap projectLang langs)
    ]

formatExtraProjects :: [Project] -> Proteome [String]
formatExtraProjects projects = do
  formatted <- traverse formatMain projects
  return $ ["", "Extra projects", ""] ++ intercalate [""] formatted

formatExtraProjectsIfNonempty :: Proteome [String]
formatExtraProjectsIfNonempty = do
  projects <- Ribo.inspect Env.projects
  case projects of
    _ : _ -> formatExtraProjects projects
    _ -> return []

formatError :: Error -> [String]
formatError (Error stamp (first:message)) = (show stamp ++ " | " ++ first) : message
formatError _ = []

formatComponentErrors :: ComponentName -> [Error] -> [String]
formatComponentErrors (ComponentName name) errors@(_ : _) =
  [name ++ "", ""] ++ (errors >>= formatError)
formatComponentErrors _ _ = []


formatErrorLog :: Errors -> [String]
formatErrorLog (Errors errors) =
  case compErrors of
    _ : _ -> ["", "Errors", ""] ++ compErrors
    _ -> []
  where
    compErrors = foldMapWithKey formatComponentErrors errors

diagnostics :: Proteome [String]
diagnostics = do
  main <- getMainProject >>= formatMain
  extra <- formatExtraProjectsIfNonempty
  confLog <- Ribo.inspect Env.configLog
  errors <- Ribo.inspect Env.errors
  return $ ["Diagnostics", "", "Main project", ""] ++ main ++ extra ++ ["", "loaded config files:"] ++
    confLog ++ formatErrorLog errors

proDiag :: CommandArguments -> Proteome ()
proDiag _ = do
  content <- diagnostics
  void $ showInScratch content (defaultScratchOptions "proteome-diagnostics")
