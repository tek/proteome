module Proteome.Diag(
  proDiag,
) where

import Data.Functor (void)
import Data.List (intercalate)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)
import qualified Ribosome.Data.Ribo as Ribo (inspect)
import Proteome.Env (getMainProject)
import Proteome.Data.Env(configLog)
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

diagnostics :: Proteome [String]
diagnostics = do
  main <- getMainProject >>= formatMain
  confLog <- Ribo.inspect $ configLog
  return $ ["Diagnostics", "", "Main project:"] ++ main ++ ["", "loaded config files:"] ++ confLog

proDiag :: CommandArguments -> Proteome ()
proDiag _ = do
  content <- diagnostics
  void $ showInScratch content (defaultScratchOptions "proteome-diagnostics")
