module Proteome.Project.Resolve where

import Control.Exception (SomeException, catch)
import Control.Monad (foldM, join)
import Control.Monad.Extra (firstJustM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader ((<=<))
import Data.Functor.Syntax ((<$$>))
import Data.List (find, null)
import Data.List.Utils (uniq)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map (toList, union)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Data.Foldable (findMapMaybeM)
import Ribosome.Data.Maybe (orElse)
import Ribosome.File (canonicalPaths)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import System.Path.Glob (glob)

import Proteome.Config (ProjectConfig(ProjectConfig), defaultTypeMarkers)
import qualified Proteome.Config as ProjectConfig (ProjectConfig(typeMarkers))
import Proteome.Data.Project (
  Project(Project),
  ProjectLang(..),
  ProjectMetadata(DirProject, VirtualProject),
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  )
import Proteome.Data.ProjectSpec (ProjectSpec(ProjectSpec))
import qualified Proteome.Data.ProjectSpec as PS (ProjectSpec(..))
import Proteome.Project (pathData)
import qualified Proteome.Settings as S

projectFromSegments :: ProjectType -> ProjectName -> ProjectRoot -> Project
projectFromSegments tpe name root =
  Project (DirProject name root (Just tpe)) [] Nothing []

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
  (root, name, tpe) <- pathData dir
  return $ projectFromSegments tpe name root

projectFromNameIn :: ProjectName -> FilePath -> IO (Maybe Project)
projectFromNameIn (ProjectName name) base = do
  candidates <- glob $ base ++ "/*/" ++ name
  traverse fromProjectRoot (listToMaybe candidates)

resolveByName :: [FilePath] -> ProjectName -> IO (Maybe Project)
resolveByName baseDirs name =
  findMapMaybeM (projectFromNameIn name) baseDirs

resolveFromDirContents :: Map ProjectType [FilePath] -> ProjectName -> ProjectRoot -> IO (Maybe Project)
resolveFromDirContents typeMarkers name projectRoot@(ProjectRoot root) =
  cons <$$> firstJustM match (Map.toList typeMarkers)
  where
    cons projectType =
      projectFromSegments projectType name projectRoot
    match (tpe, patterns) =
      (tpe <$) . listToMaybe <$> catch @SomeException (getDirectoryFiles root patterns) (const $ return [])

resolveByRoot :: ProjectConfig -> ProjectName -> [ProjectSpec] -> ProjectRoot -> IO (Maybe Project)
resolveByRoot (ProjectConfig _ _ typeMarkers _ _) name explicit root =
  maybe (resolveFromDirContents typeMarkers name root) (return . Just . projectFromSpec) fromExplicit
  where
    fromExplicit = find (hasProjectRoot root) explicit

augment :: (Eq a, Ord k) => Map k [a] -> k -> [a] -> [a]
augment m tpe as =
  case m !? tpe of
    Just extra -> uniq $ as ++ extra
    Nothing -> as

augmentTypes :: ProjectConfig -> ProjectType -> [ProjectType] -> [ProjectType]
augmentTypes (ProjectConfig _ typeMap _ _ _) =
  augment typeMap

realLang :: ProjectConfig -> ProjectType -> ProjectLang
realLang (ProjectConfig _ _ _ langMap _) t@(ProjectType tpe) =
  fromMaybe (ProjectLang tpe) (langMap !? t)

augmentLangs :: ProjectConfig -> ProjectLang -> [ProjectLang] -> [ProjectLang]
augmentLangs (ProjectConfig _ _ _ _ langsMap) =
  augment langsMap

augmentFromConfig :: ProjectConfig -> Project -> Project
augmentFromConfig config (Project meta@(DirProject _ _ (Just tpe)) types lang langs) =
  Project meta (augmentTypes config tpe types) (Just realLang') (augmentLangs config realLang' langs)
  where
    realLang' = fromMaybe (realLang config tpe) lang
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
  byName <- if isJust root then return Nothing else resolveByName baseDirs name
  byRoot <- join <$> traverse (resolveByRoot config name explicit) root
  let byNameOrVirtual = fromMaybe (virtualProject name) byName
  let byTypeOrName = fromMaybe byNameOrVirtual (join byType)
  let project = fromMaybe byTypeOrName byRoot
  return $ augmentFromConfig config project

projectConfig :: Ribo e ProjectConfig
projectConfig = do
  config <- setting S.projectConfig
  return config { ProjectConfig.typeMarkers = Map.union (ProjectConfig.typeMarkers config) defaultTypeMarkers }

resolveProjectFromConfig :: Maybe ProjectRoot -> ProjectName -> Maybe ProjectType -> Ribo e Project
resolveProjectFromConfig root name tpe = do
  baseDirs <- (canonicalPaths <=< setting) S.projectBaseDirs
  -- typeDirs <- setting S.projectTypeDirs
  explicit <- setting S.projects
  config <- projectConfig
  liftIO $ resolveProject baseDirs explicit config root name tpe
