module Proteome.Project.Resolve where

import Control.Monad (foldM)
import Control.Monad.Extra (firstJustM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List (nub)
import Data.List.Extra (firstJust)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (toList, union)
import Exon (exon)
import qualified Log
import Path (Abs, Dir, Path, dirname, isProperPrefixOf, parent, parseRelDir, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesDirExist)
import Ribosome (Settings)
import qualified Ribosome.Settings as Settings
import System.FilePath.Glob (globDir1)
import qualified System.FilePath.Glob as Glob (compile)
import System.FilePattern.Directory (getDirectoryFiles)

import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project (Project))
import qualified Proteome.Data.ProjectConfig as ProjectConfig
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectLang (ProjectLang (ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot), unProjectRoot)
import Proteome.Data.ProjectSpec (ProjectSpec (ProjectSpec))
import qualified Proteome.Data.ProjectSpec as PS (ProjectSpec (..))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import qualified Proteome.Data.ResolveError as ResolveError (ResolveError (..))
import Proteome.Data.ResolveError (ResolveError)
import Proteome.Path (dropSlash, parseAbsDirMaybe, rootPathSegment)
import Proteome.Project (pathData)
import qualified Proteome.Settings as Settings (projectConfig, projects)

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

matchProjectBases :: [Path Abs Dir] -> ProjectRoot -> Bool
matchProjectBases baseDirs (ProjectRoot root) = (parent . parent) root `elem` baseDirs

byProjectBaseSubpath ::
  Members [Stop ResolveError, Embed IO] r =>
  ProjectName ->
  ProjectType ->
  Path Abs Dir ->
  Sem r (Maybe Project)
byProjectBaseSubpath n@(ProjectName name) t@(ProjectType tpe) base = do
  tpePath <- stopEitherAs (ResolveError.ParsePath tpe) $ parseRelDir (toString tpe)
  namePath <- stopEitherAs (ResolveError.ParsePath name) $ parseRelDir (toString name)
  let root = base </> tpePath </> namePath
  exists <- doesDirExist root
  pure $ if exists then Just $ projectFromSegments t n (ProjectRoot root) else Nothing

byProjectBasesSubpath ::
  Members [Stop ResolveError, Embed IO] r =>
  [Path Abs Dir] ->
  ProjectName ->
  ProjectType ->
  Sem r (Maybe Project)
byProjectBasesSubpath baseDirs name tpe =
  foldM subpath Nothing baseDirs
  where
    subpath (Just p) _ = pure (Just p)
    subpath Nothing a = byProjectBaseSubpath name tpe a

virtualProject :: ProjectName -> Project
virtualProject name =
  Project (VirtualProject name) [] Nothing []

resolveByTypeAndPath :: [Path Abs Dir] -> ProjectName -> ProjectType -> ProjectRoot -> Maybe Project
resolveByTypeAndPath baseDirs name tpe root =
  if matchProjectBases baseDirs root then Just (projectFromSegments tpe name root) else Nothing

resolveByType ::
  Members [Stop ResolveError, Embed IO] r =>
  [Path Abs Dir] ->
  [ProjectSpec] ->
  ProjectName ->
  ProjectType ->
  Sem r (Maybe Project)
resolveByType baseDirs explicit name tpe = do
  byBaseSubpath <- byProjectBasesSubpath baseDirs name tpe
  pure (byBaseSubpath <|> projectFromSpec <$> byTypeName)
  where
    byTypeName =
      byProjectTypeName explicit name tpe

fromProjectRoot :: Path Abs Dir -> Project
fromProjectRoot dir =
  projectFromSegments tpe name root
  where
    (root, name, tpe) = pathData dir

projectFromNameIn ::
  Members [Stop ResolveError, Embed IO] r =>
  ProjectName ->
  Path Abs Dir ->
  Sem r (Maybe Project)
projectFromNameIn (ProjectName name) base =
  fmap fromProjectRoot . join . find isJust <$> matches
  where
    matches =
      fmap (parseAbsDirMaybe . toText) <$> glob
    glob =
      embed $ globDir1 (Glob.compile ("*/" <> toString name)) (toFilePath base)

resolveByName ::
  Members [Stop ResolveError, Embed IO] r =>
  [Path Abs Dir] ->
  ProjectName ->
  Sem r (Maybe Project)
resolveByName baseDirs name =
  firstJustM (projectFromNameIn name) baseDirs

globDir ::
  Members [Stop ResolveError, Embed IO] r =>
  Path Abs Dir ->
  [Text] ->
  Sem r (Maybe FilePath)
globDir root patterns =
  (head =<<) <$> tryMaybe (getDirectoryFiles (toFilePath root) (toString <$> patterns))

resolveFromDirContents ::
  Members [Stop ResolveError, Embed IO] r =>
  Map ProjectType [Text] ->
  ProjectName ->
  ProjectRoot ->
  Sem r (Maybe Project)
resolveFromDirContents typeMarkers name projectRoot@(ProjectRoot root) =
  fmap cons <$> firstJustM match (Map.toList typeMarkers)
  where
    cons projectType =
      projectFromSegments projectType name projectRoot
    match (tpe, patterns) =
      (tpe <$) <$> globDir root patterns

resolveByRoot ::
  Members [Stop ResolveError, Embed IO] r =>
  ProjectConfig ->
  ProjectName ->
  [ProjectSpec] ->
  ProjectRoot ->
  Sem r (Maybe Project)
resolveByRoot (ProjectConfig _ _ _ _ typeMarkers _ _) name explicit root =
  maybe (resolveFromDirContents typeMarkers name root) (pure . Just . projectFromSpec) fromExplicit
  where
    fromExplicit = find (hasProjectRoot root) explicit

augment ::
  Eq a =>
  Ord k =>
  Map k [a] ->
  k ->
  [a] ->
  [a]
augment m tpe as =
  case m !? tpe of
    Just extra -> nub $ as ++ extra
    Nothing -> as

augmentTypes :: ProjectConfig -> ProjectType -> [ProjectType] -> [ProjectType]
augmentTypes (ProjectConfig _ _ _ typeMap _ _ _) =
  augment typeMap

resolveLanguage :: ProjectConfig -> ProjectType -> ProjectLang
resolveLanguage (ProjectConfig _ _ _ _ _ langMap _) t@(ProjectType tpe) =
  fromMaybe (ProjectLang tpe) (langMap !? t)

augmentLangs :: ProjectConfig -> ProjectLang -> [ProjectLang] -> [ProjectLang]
augmentLangs (ProjectConfig _ _ _ _ _ _ langsMap) =
  augment langsMap

augmentFromConfig :: ProjectConfig -> Project -> Project
augmentFromConfig config (Project meta@(DirProject _ _ (Just tpe)) types lang langs) =
  Project meta (augmentTypes config tpe types) (Just lang') (augmentLangs config lang' langs)
  where
    lang' = fromMaybe (resolveLanguage config tpe) lang
augmentFromConfig _ project =
  project

fromName ::
  Members [Stop ResolveError, Log, Embed IO] r =>
  [ProjectSpec] ->
  ProjectConfig ->
  ProjectName ->
  Maybe ProjectType ->
  Sem r Project
fromName explicit config name tpe = do
  let baseDirs = ProjectConfig.baseDirs config
  byType <- join <$> traverse (resolveByType baseDirs explicit name) tpe
  byName <- resolveByName baseDirs name
  let byNameOrVirtual = fromMaybe (virtualProject name) byName
  let project = fromMaybe byNameOrVirtual byType
  Log.debug (logMsg byType byName)
  pure (augmentFromConfig config project)
  where
    logMsg byType byName =
      [exon|resolved project: byType(#{show byType}) byName(#{show byName})|]

fromNameSettings ::
  Members [Settings, Stop ResolveError, Log, Embed IO] r =>
  ProjectName ->
  Maybe ProjectType ->
  Sem r Project
fromNameSettings name tpe = do
  explicit <- Settings.get Settings.projects
  config <- projectConfig
  fromName explicit config name tpe

projectConfig ::
  Member Settings r =>
  Sem r ProjectConfig
projectConfig =
  (#typeMarkers %~ (`Map.union` defaultTypeMarkers)) <$> Settings.get Settings.projectConfig

rootExplicit :: [ProjectSpec] -> ProjectRoot -> Maybe Project
rootExplicit explicit root =
  projectFromSpec <$> find (hasProjectRoot root) explicit

rootProjectTypes ::
  Map ProjectType [Path Abs Dir] ->
  ProjectName ->
  ProjectRoot ->
  Maybe Project
rootProjectTypes tpes name root@(ProjectRoot rootDir) =
  cons . fst <$> find (elem rootDir . snd) (Map.toList tpes)
  where
    cons tpe =
      projectFromSegments tpe name root

rootTypeDirs ::
  Map ProjectType [Path Abs Dir] ->
  ProjectName ->
  ProjectRoot ->
  Maybe Project
rootTypeDirs types name root@(ProjectRoot rootDir) =
  cons . fst <$> find (matchType . snd) (Map.toList types)
  where
    cons tpe =
      projectFromSegments tpe name root
    matchType =
      any match
    match base = do
      isProperPrefixOf base rootDir

rootBaseDirs ::
  [Path Abs Dir] ->
  ProjectName ->
  ProjectRoot ->
  Maybe Project
rootBaseDirs bases name root@(ProjectRoot rootDir) =
  firstJust match bases
  where
    match base = do
      rel <- stripProperPrefix base rootDir
      let tpe = rootPathSegment (parent rel)
      if rel /= tpe
      then Just (projectFromSegments (ProjectType (dropSlash tpe)) name root)
      else Nothing

projectName :: ProjectRoot -> ProjectName
projectName =
  ProjectName . dropSlash . dirname . unProjectRoot

firstJustMOr ::
  Monad m =>
  a ->
  [m (Maybe a)] ->
  m a
firstJustMOr fallback =
  fmap (fromMaybe fallback) . runMaybeT . asum @[] . fmap MaybeT

fromRoot ::
  Members [Stop ResolveError, Embed IO] r =>
  [ProjectSpec] ->
  ProjectConfig ->
  ProjectRoot ->
  Sem r Project
fromRoot explicit config@ProjectConfig {..} root = do
  let
    byRoot =
      asum @[] [
        rootExplicit explicit root,
        rootProjectTypes projectTypes name root,
        rootTypeDirs typeDirs name root,
        rootBaseDirs baseDirs name root
        ]
  project <- firstJustMOr (virtualProject name) [
    pure byRoot,
    resolveFromDirContents typeMarkers name root
    ]
  pure (augmentFromConfig config project)
  where
    name =
      projectName root

fromRootSettings ::
  Members [Settings, Stop ResolveError, Embed IO] r =>
  ProjectRoot ->
  Sem r Project
fromRootSettings root = do
  explicit <- Settings.get Settings.projects
  config <- projectConfig
  fromRoot explicit config root
