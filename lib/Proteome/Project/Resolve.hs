{-# LANGUAGE NamedFieldPuns #-}

module Proteome.Project.Resolve(
  resolveProject,
) where

import Data.List (find)
import Data.List.Utils (uniq)
import Data.Maybe (fromMaybe)
import Data.Map.Strict ((!?), Map)
import System.FilePath (takeDirectory)
import Ribosome.Data.Maybe (orElse)
import Proteome.Config (ProjectConfig(ProjectConfig))
import Proteome.Data.Project (
  Project(Project),
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectLang(..),
  ProjectMetadata(DirProject, VirtualProject),
  )
import Proteome.Data.ProjectSpec (ProjectSpec(ProjectSpec))
import qualified Proteome.Data.ProjectSpec as PS (ProjectSpec(..))

projectFromSegments :: ProjectType -> ProjectName -> ProjectRoot -> Project
projectFromSegments tpe name root =
  Project (DirProject name root (Just tpe)) [] (Just (ProjectLang (projectType tpe))) []

projectFromSpec :: ProjectSpec -> Project
projectFromSpec (ProjectSpec name root tpe types lang langs) =
  Project (DirProject name root tpe) types lang langs

hasProjectRoot :: ProjectRoot -> ProjectSpec -> Bool
hasProjectRoot root spec = root == PS.root spec

hasProjectTypeName :: ProjectType -> ProjectName -> ProjectSpec -> Bool
hasProjectTypeName tpe' name' (ProjectSpec name _ (Just tpe) _ _ _) =
  name' == name && tpe' == tpe
hasProjectTypeName _ _ _ = False

byProjectTypeName :: [ProjectSpec] -> ProjectName -> ProjectType -> Maybe ProjectSpec
byProjectTypeName specs name tpe = find (hasProjectTypeName tpe name) specs

byProjectBases :: [FilePath] -> ProjectRoot -> Bool
byProjectBases baseDirs (ProjectRoot root) = (takeDirectory . takeDirectory) root `elem` baseDirs

virtualProject :: ProjectName -> Project
virtualProject name = Project (VirtualProject name) [] Nothing []

resolveByType :: [FilePath] -> [ProjectSpec] -> ProjectRoot -> ProjectName -> ProjectType -> Maybe Project
resolveByType baseDirs explicit root name tpe =
  orElse (if byPath then Just (projectFromSegments tpe name root) else Nothing) (fmap projectFromSpec byTypeName)
  where
    byTypeName = byProjectTypeName explicit name tpe
    byPath = byProjectBases baseDirs root

resolveByRoot :: [ProjectSpec] -> ProjectRoot -> Maybe Project
resolveByRoot explicit root =
  fmap projectFromSpec byRoot
  where
    byRoot = find (hasProjectRoot root) explicit

augment :: Eq a => Map ProjectType [a] -> ProjectType -> [a] -> [a]
augment m tpe as =
  case m !? tpe of
    Just extra -> uniq $ as ++ extra
    Nothing -> as

augmentTypes :: ProjectConfig -> ProjectType -> [ProjectType] -> [ProjectType]
augmentTypes (ProjectConfig _ typeMap _) =
  augment typeMap

augmentLangs :: ProjectConfig -> ProjectType -> [ProjectLang] -> [ProjectLang]
augmentLangs (ProjectConfig _ _ langMap) =
  augment langMap

augmentFromConfig :: ProjectConfig -> Project -> Project
augmentFromConfig config (Project meta@(DirProject _ _ (Just tpe)) types lang langs) =
  Project meta (augmentTypes config tpe types) lang (augmentLangs config tpe langs)
augmentFromConfig _ project = project

resolveProject ::
  [FilePath] ->
  [ProjectSpec] ->
  ProjectConfig ->
  ProjectRoot ->
  ProjectName ->
  Maybe ProjectType ->
  Project
resolveProject baseDirs explicit config root name tpe =
  augmentFromConfig config project
  where
    project = fromMaybe byTypeOrVirtual byRoot
    byTypeOrVirtual = fromMaybe (virtualProject name) byType
    byType = tpe >>= resolveByType baseDirs explicit root name
    byRoot = resolveByRoot explicit root
