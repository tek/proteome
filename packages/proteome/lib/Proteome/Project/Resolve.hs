module Proteome.Project.Resolve where

import qualified Control.Lens as Lens (over)
import Control.Lens ((^.))
import Control.Monad (foldM)
import Control.Monad.Extra (firstJustM)
import Data.List (nub)
import Data.List.Extra (firstJust)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map (toList, union)
import Path (Abs, Dir, Path, dirname, isProperPrefixOf, parent, parseRelDir, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesDirExist)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Exception (catchAnyAs)
import Ribosome.Data.Foldable (findMapMaybeM)
import Ribosome.Data.SettingError (SettingError)
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
import Proteome.Data.ResolveError (ResolveError)
import qualified Proteome.Data.ResolveError as ResolveError (ResolveError (..))
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
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  ProjectName ->
  ProjectType ->
  Path Abs Dir ->
  m (Maybe Project)
byProjectBaseSubpath n@(ProjectName name) t@(ProjectType tpe) base = do
  tpePath <- hoistEitherAs (ResolveError.ParsePath tpe) $ parseRelDir (toString tpe)
  namePath <- hoistEitherAs (ResolveError.ParsePath name) $ parseRelDir (toString name)
  let root = base </> tpePath </> namePath
  exists <- doesDirExist root
  pure $ if exists then Just $ projectFromSegments t n (ProjectRoot root) else Nothing

byProjectBasesSubpath ::
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  [Path Abs Dir] ->
  ProjectName ->
  ProjectType ->
  m (Maybe Project)
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
  MonadIO m =>
  MonadDeepError e ResolveError m =>
  [Path Abs Dir] ->
  [ProjectSpec] ->
  ProjectName ->
  ProjectType ->
  m (Maybe Project)
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
  ∀ m .
  MonadIO m =>
  ProjectName ->
  Path Abs Dir ->
  m (Maybe Project)
projectFromNameIn (ProjectName name) base =
  fmap fromProjectRoot <$> join . find isJust <$> matches
  where
    matches =
      fmap (parseAbsDirMaybe . toText) <$> glob
    glob =
      liftIO $ globDir1 (Glob.compile ("*/" <> toString name)) (toFilePath base)

resolveByName ::
  MonadIO m =>
  [Path Abs Dir] ->
  ProjectName ->
  m (Maybe Project)
resolveByName baseDirs name =
  findMapMaybeM (projectFromNameIn name) baseDirs

globDir ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  [Text] ->
  m (Maybe FilePath)
globDir root patterns =
  listToMaybe <$> catchAnyAs [] (liftIO $ getDirectoryFiles rootS patternsS)
  where
    patternsS =
      toString <$> patterns
    rootS =
      toFilePath root

resolveFromDirContents ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Map ProjectType [Text] ->
  ProjectName ->
  ProjectRoot ->
  m (Maybe Project)
resolveFromDirContents typeMarkers name projectRoot@(ProjectRoot root) =
  fmap cons <$> firstJustM match (Map.toList typeMarkers)
  where
    cons projectType =
      projectFromSegments projectType name projectRoot
    match (tpe, patterns) =
      (tpe <$) <$> globDir root patterns

resolveByRoot ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ProjectConfig ->
  ProjectName ->
  [ProjectSpec] ->
  ProjectRoot ->
  m (Maybe Project)
resolveByRoot (ProjectConfig _ _ _ _ typeMarkers _ _) name explicit root =
  maybe (resolveFromDirContents typeMarkers name root) (pure . Just . projectFromSpec) fromExplicit
  where
    fromExplicit = find (hasProjectRoot root) explicit

augment :: (Eq a, Ord k) => Map k [a] -> k -> [a] -> [a]
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
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e ResolveError m =>
  [ProjectSpec] ->
  ProjectConfig ->
  ProjectName ->
  Maybe ProjectType ->
  m Project
fromName explicit config name tpe = do
  let baseDirs = config ^. ProjectConfig.baseDirs
  byType <- join <$> traverse (resolveByType baseDirs explicit name) tpe
  byName <- resolveByName baseDirs name
  let byNameOrVirtual = fromMaybe (virtualProject name) byName
  let project = fromMaybe byNameOrVirtual byType
  logDebug @Text $ logMsg byType byName
  pure (augmentFromConfig config project)
  where
    logMsg byType byName =
      [exon|resolved project: byType(#{show byType}) byName(#{show byName})|]

fromNameSettings ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e ResolveError m =>
  MonadDeepError e SettingError m =>
  ProjectName ->
  Maybe ProjectType ->
  m Project
fromNameSettings name tpe = do
  explicit <- setting Settings.projects
  config <- projectConfig
  fromName explicit config name tpe

projectConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  m ProjectConfig
projectConfig =
  Lens.over ProjectConfig.typeMarkers (`Map.union` defaultTypeMarkers) <$> setting Settings.projectConfig

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
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  [ProjectSpec] ->
  ProjectConfig ->
  ProjectRoot ->
  m Project
fromRoot explicit config root = do
  let
    byRoot =
      asum @[] [
        rootExplicit explicit root,
        rootProjectTypes (config ^. ProjectConfig.projectTypes) name root,
        rootTypeDirs (config ^. ProjectConfig.typeDirs) name root,
        rootBaseDirs (config ^. ProjectConfig.baseDirs) name root
        ]
  project <- firstJustMOr (virtualProject name) [
    pure byRoot,
    resolveFromDirContents (config ^. ProjectConfig.typeMarkers) name root
    ]
  pure (augmentFromConfig config project)
  where
    name =
      projectName root

fromRootSettings ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  ProjectRoot ->
  m Project
fromRootSettings root = do
  explicit <- setting Settings.projects
  config <- projectConfig
  fromRoot explicit config root
