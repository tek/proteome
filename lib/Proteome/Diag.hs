module Proteome.Diag(
  proDiag,
) where

import Data.Functor (void)
import Data.List (intercalate)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)
import Proteome.Env (getMainProject)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project (Project),
  ProjectLang(..),
  ProjectRoot(ProjectRoot),
  ProjectName(ProjectName),
  ProjectType(..),
  ProjectMetadata (DirProject, VirtualProject),
  )

formatLang :: Maybe ProjectLang -> String
formatLang (Just (ProjectLang lang)) = lang
formatLang Nothing = "none"

formatType :: Maybe ProjectType -> String
formatType (Just (ProjectType tpe)) = tpe
formatType Nothing = "none"

formatMeta :: ProjectMetadata -> [String]
formatMeta (VirtualProject (ProjectName name)) = ["name: " ++ name]
formatMeta (DirProject (ProjectName name) (ProjectRoot root) tpe) =
  [
    "name: " ++ name,
    "root: " ++ root,
    "type: " ++ formatType tpe
  ]

formatMain :: Project -> [String]
formatMain (Project meta types lang langs) =
  formatMeta meta ++ [
    "types: " ++ intercalate ", " (fmap projectType types),
    "main language: " ++ formatLang lang,
    "languages: " ++ intercalate ", " (fmap projectLang langs)
    ]

diagnostics :: Proteome [String]
diagnostics = do
  main <- fmap formatMain getMainProject
  return $ ["Diagnostics", "", "Main project:"] ++ main

proDiag :: CommandArguments -> Proteome ()
proDiag _ = do
  content <- diagnostics
  void $ showInScratch content (defaultScratchOptions "proteome-diagnostics")
