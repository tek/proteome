{-# LANGUAGE NamedFieldPuns #-}

module Proteome.Project.Resolve(
  resolveProject,
  resolveProjectFromConfig,
) where

import Control.Monad (foldM, join)
import Control.Monad.Reader ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.List.Utils (uniq)
import Data.Maybe (fromMaybe)
import Data.Map.Strict ((!?), Map)
import Safe (headMay)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.Path.Glob (glob)
import Ribosome.File (canonicalPaths)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.Foldable (findMapMaybeM)
import Ribosome.Data.Maybe (orElse)
import Ribosome.Data.Ribo (Ribo)
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
import Proteome.Project (pathData)
import qualified Proteome.Settings as S

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

matchProjectBases :: [FilePath] -> ProjectRoot -> Bool
matchProjectBases baseDirs (ProjectRoot root) = (takeDirectory . takeDirectory) root `elem` baseDirs

byProjectBaseSubpath :: ProjectName -> ProjectType -> FilePath -> IO (Maybe Project)
byProjectBaseSubpath n@(ProjectName name) t@(ProjectType tpe) base = do
  exists <- doesDirectoryExist root
  return $ if exists then Just $ projectFromSegments t n (ProjectRoot root) else Nothing
  where root = base </> tpe </> name

byProjectBasesSubpath :: [FilePath] -> ProjectName -> ProjectType -> IO (Maybe Project)
byProjectBasesSubpath baseDirs name tpe =
  foldM subpath Nothing baseDirs
  where
    subpath (Just p) _ = return (Just p)
    subpath Nothing a = byProjectBaseSubpath name tpe a

virtualProject :: ProjectName -> Project
virtualProject name = Project (VirtualProject name) [] Nothing []

resolveByTypeAndPath :: [FilePath] -> ProjectName -> ProjectType -> ProjectRoot -> Maybe Project
resolveByTypeAndPath baseDirs name tpe root =
  if matchProjectBases baseDirs root then Just (projectFromSegments tpe name root) else Nothing

resolveByType :: [FilePath] -> [ProjectSpec] -> Maybe ProjectRoot -> ProjectName -> ProjectType -> IO (Maybe Project)
resolveByType baseDirs explicit root name tpe = do
  byBaseSubpath <- byProjectBasesSubpath baseDirs name tpe
  return $ orElse (orElse byPath byBaseSubpath) (fmap projectFromSpec byTypeName)
  where
    byTypeName = byProjectTypeName explicit name tpe
    byPath = root >>= resolveByTypeAndPath baseDirs name tpe

fromProjectRoot :: FilePath -> IO Project
fromProjectRoot dir = do
  (root, name, tpe) <- pathData (Just dir)
  return $ projectFromSegments tpe name root

projectFromNameIn :: ProjectName -> FilePath -> IO (Maybe Project)
projectFromNameIn (ProjectName name) base = do
  candidates <- glob $ base ++ "/*/" ++ name
  mapM fromProjectRoot (headMay candidates)

resolveByName :: [FilePath] -> ProjectName -> IO (Maybe Project)
resolveByName baseDirs name =
  findMapMaybeM (projectFromNameIn name) baseDirs

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
  Maybe ProjectRoot ->
  ProjectName ->
  Maybe ProjectType ->
  IO Project
resolveProject baseDirs explicit config root name tpe = do
  byType <- traverse (resolveByType baseDirs explicit root name) tpe
  byName <- resolveByName baseDirs name
  let byNameOrVirtual = fromMaybe (virtualProject name) byName
  let byTypeOrName = fromMaybe byNameOrVirtual (join byType)
  let byRoot = root >>= resolveByRoot explicit
  let project = fromMaybe byTypeOrName byRoot
  return $ augmentFromConfig config project

resolveProjectFromConfig :: Maybe ProjectRoot -> ProjectName -> Maybe ProjectType -> Ribo e Project
resolveProjectFromConfig root name tpe = do
  baseDirs <- (canonicalPaths <=< setting) S.projectBaseDirs
  -- typeDirs <- setting S.projectTypeDirs
  explicit <- setting S.projects
  config <- setting S.projectConfig
  liftIO $ resolveProject baseDirs explicit config root name tpe
