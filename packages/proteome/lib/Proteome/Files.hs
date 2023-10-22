module Proteome.Files where

import Control.Monad (foldM)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.List.NonEmpty.Zipper as Zipper
import qualified Data.Text as Text
import Lens.Micro.Extras (view)
import qualified Log
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  SomeBase (Abs, Rel),
  isProperPrefixOf,
  parent,
  parseAbsDir,
  parseRelDir,
  parseSomeFile,
  reldir,
  stripProperPrefix,
  toFilePath,
  (</>),
  )
import Path.IO (createDirIfMissing, doesDirExist, listDirRel)
import Ribosome (
  Handler,
  Report,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  mapReport,
  pathText,
  resumeReport,
  )
import Ribosome.Api (currentBufferPath, nvimGetOption)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.ScratchOptions (ScratchOptions (filetype, name, syntax))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Host.Data.Args (ArgList (ArgList))
import Ribosome.Menu (
  Filter (Filter),
  FilterMethod (Fuzzy),
  MenuApp,
  MenuItem,
  MenuWidget,
  Prompt (..),
  PromptMode,
  PromptModes (OnlyInsert),
  PromptText (PromptText),
  WindowMenus,
  WindowOptions,
  insert,
  menuOk,
  menuState,
  menuSuccess,
  menuUpdatePrompt,
  modal,
  use,
  windowMenu,
  withInsert,
  withSelection,
  (%=),
  )
import Ribosome.Menu.Action (menuRenderLine)
import Ribosome.Menu.Data.WithCursor (WithCursor)
import Ribosome.Menu.MenuState (mode)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import qualified Ribosome.Settings as Settings
import Streamly.Prelude (SerialT)
import Text.Regex.PCRE.Light (Regex, compileM)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError (..))
import qualified Proteome.Data.FilesState as FilesState
import Proteome.Data.FilesState (
  BaseDir (BaseDir),
  FileSegments,
  FilesMode (FilesMode),
  FilesState (FilesState),
  Segment (Full),
  fileSegments,
  )
import Proteome.Files.Source (files)
import Proteome.Files.Syntax (filesSyntax)
import Proteome.Menu (handleResult)
import qualified Proteome.Settings as Settings

data FileAction =
  Create (Path Abs File)
  |
  Edit (NonEmpty (Path Abs File))
  |
  NoAction
  deriving stock (Eq, Show)

editFile ::
  MenuWidget FilesState r FileAction
editFile =
  withSelection (pure . Edit . fmap (view #path))

setBase ::
  Member (State (WithCursor FilesState)) r =>
  BaseDir ->
  Sem r ()
setBase base =
  #state . #bases %= \ b -> fromMaybe b (Zipper.findRight base (Zipper.start b))

matchingDirs ::
  Member (Embed IO) r =>
  [BaseDir] ->
  Path Rel Dir ->
  Sem r [(BaseDir, Path Abs Dir)]
matchingDirs bases path =
  filterM match withSubpaths
  where
    match (_, subpath) =
      fromRight False <$> tryAny (doesDirExist subpath)
    withSubpaths =
      bases <&> \ b -> (b, b ^. #absolute </> path)

dirsWithPrefix ::
  Member (Embed IO) r =>
  Text ->
  BaseDir ->
  Path Abs Dir ->
  Sem r [(BaseDir, Path Rel Dir)]
dirsWithPrefix (Text.toLower -> prefix) base dir = do
  subs <- fst <$> listDirRel dir
  let matches = filter (Text.isPrefixOf prefix . Text.toLower . toText . toFilePath) subs
  pure ((base,) <$> matches)

-- |Search all dirs in @bases@ for relative paths starting with @text@.
-- First, split the last path segment (after /) off and collect the subdirectories of @bases@ that start with the
-- remainder. If there is no / in the text, parsing the remainder fails with 'Nothing' and the @bases@ themselves are
-- used.
-- Then, search the resulting dirs for subdirs starting with the last segment.
-- Return the remainder and the relative subdir paths.
matchingPaths ::
  Member (Embed IO) r =>
  [BaseDir] ->
  Text ->
  Sem r (Text, [(BaseDir, Path Rel Dir)])
matchingPaths bases text' =
  (subpath,) . join <$> (traverse (uncurry (dirsWithPrefix prefix)) =<< dirs)
  where
    subpath =
      maybe "" (toText . toFilePath) dir
    dirs =
      maybe (pure fallback) (matchingDirs bases) dir
    fallback =
      bases <&> \ b -> (b, b ^. #absolute)
    (dir, prefix) =
      first (parseRelDir . toString) $ Text.breakOnEnd "/" text'

commonPrefix :: [Text] -> Maybe Text
commonPrefix (h : t) =
  foldM (\ p a -> view _1 <$> Text.commonPrefixes p a) h t
commonPrefix a =
  listToMaybe a

tabComplete ::
  Member (Embed IO) r =>
  [BaseDir] ->
  Text ->
  Sem r (Maybe (BaseDir, Text))
tabComplete bases promptText = do
  existingBases <- filterM (doesDirExist . view #absolute) bases
  (subpath, paths) <- matchingPaths existingBases promptText
  let
    base = fst <$> head paths
    prefix = mappend subpath <$> commonPrefix (pathText . snd <$> paths)
  pure (pairA base prefix)

tabUpdatePrompt ::
  PromptMode ->
  Text ->
  Prompt
tabUpdatePrompt st prefix =
  Prompt (fromIntegral (Text.length prefix)) st (PromptText prefix)

matchingBase ::
  [BaseDir] ->
  Text ->
  Maybe BaseDir
matchingBase bases prefix =
  trs (bases, prefix)
  Nothing

tab ::
  Member (Embed IO) r =>
  MenuWidget FilesState r FileAction
tab = do
  menuState do
    Prompt _ promptState (PromptText promptText) <- asks (.prompt)
    bases <- toList <$> use (#state . #bases)
    tabComplete bases promptText >>= \case
      Just (base, prefix) -> do
        setBase base
        menuUpdatePrompt (tabUpdatePrompt promptState prefix)
      Nothing ->
        menuOk

createAndEditFile ::
  Members [Rpc, Stop FilesError, Embed IO] r =>
  Path Abs File ->
  Sem r ()
createAndEditFile path = do
  stopTryAny (const err) create
  edit path
  where
    err =
      FilesError.CouldntCreateDir (toText (toFilePath dir))
    create =
      createDirIfMissing True dir
    dir =
      parent path

existingSubdirCount ::
  Member (Embed IO) r =>
  [Text] ->
  Path Abs Dir ->
  Sem r Int
existingSubdirCount =
  loop 0
  where
    loop count [] _ =
      pure count
    loop count (h : t) dir =
      case parseRelDir (toString h) of
        Right f ->
          ifM (doesDirExist sub) (loop (count + 1) t sub) (pure count)
          where sub = dir </> f
        Left _ ->
          pure count

currentBase ::
  Member (State (WithCursor FilesState)) r =>
  Sem r BaseDir
currentBase =
  Zipper.current <$> use (#state . #bases)

createFile ::
  Members [Stop FilesError, Embed IO] r =>
  MenuWidget FilesState r FileAction
createFile = do
  PromptText promptText <- asks (view (#prompt . #text))
  base <- menuState currentBase
  let
    path =
      parseSomeFile (toString promptText) <&> \case
        Abs p -> p
        Rel p -> base ^. #absolute </> p
  maybe (err promptText) (menuSuccess . Create) path
  where
    err = stop . FilesError.InvalidFilePath

cycleSegment :: MenuWidget FilesState r FileAction
cycleSegment =
  menuState do
    mode . #segment %= FilesState.cycle
    menuRenderLine

cycleBase :: MenuWidget FilesState r FileAction
cycleBase =
  menuState do
    #state . #bases %= FilesState.cycleBase
    menuRenderLine

-- TODO unify with tabUpdatePrompt
insertFileDir ::
  Member Log r =>
  MenuWidget FilesState r FileAction
insertFileDir =
  menuState $ use (#state . #bufferPath) >>= \case
    Right (bufDir, base) -> do
      let dir = pathText bufDir
      Prompt _ m _ <- asks (.prompt)
      setBase base
      menuUpdatePrompt (Prompt (fromIntegral (Text.length dir)) m (PromptText dir))
    Left reason -> do
      Log.info reason
      menuOk

actions ::
  Members [Stop FilesError, Log, Embed IO] r =>
  MenuApp FilesState r FileAction
actions =
  [
    (withInsert "<cr>", editFile),
    (insert "<tab>", tab),
    (insert "<c-y>", createFile),
    (withInsert "<c-s>", cycleSegment),
    (withInsert "<c-d>", insertFileDir),
    (withInsert "<c-b>", cycleBase)
  ]

parsePath :: Path Abs Dir -> Text -> Maybe (Path Abs Dir)
parsePath _ path | Text.take 1 path == "/" =
  parseAbsDir (toString path)
parsePath cwd path =
  (cwd </>) <$> parseRelDir (toString path)

readRegex ::
  Member (Stop FilesError) r =>
  Text ->
  Text ->
  Sem r Regex
readRegex name rgx =
  stopNote (FilesError.BadRegex name rgx) (eitherToMaybe (compileM (encodeUtf8 rgx) mempty))

readRegexs ::
  Members [Settings, Stop FilesError] r =>
  Setting [Text] ->
  Sem r [Regex]
readRegexs s@(Setting name _ _) =
  traverse (readRegex name) =<< Settings.get s

filesConfig ::
  Members [Rpc, Settings, Stop FilesError] r =>
  Sem r FilesConfig
filesConfig =
  FilesConfig <$> useRg <*> hidden <*> fs <*> dirs <*> wildignore
  where
    useRg =
      Settings.get Settings.filesUseRg
    hidden =
      Settings.get Settings.filesExcludeHidden
    fs =
      readRegexs Settings.filesExcludeFiles
    dirs =
      readRegexs Settings.filesExcludeDirectories
    wildignore =
      Text.splitOn "," <$> nvimGetOption "wildignore"

fileAction ::
  Members [Rpc, Stop FilesError, Stop Report, Embed IO] r =>
  FileAction ->
  Sem r ()
fileAction = \case
  Create path ->
    createAndEditFile path
  Edit paths ->
    traverse_ edit paths
  NoAction ->
    unit

type FilesStack =
  [
    WindowMenus FilesState !! RpcError,
    Log,
    Async,
    Embed IO
  ]

resolveBufPath ::
  Member Rpc r =>
  NonEmpty BaseDir ->
  Sem r (Either Text (Path Rel Dir, BaseDir))
resolveBufPath bases =
  runError do
    path <- parent <$> (note "Current buffer is not associated with a path" =<< currentBufferPath)
    (rel, base) <- note "Current buffer is not in any of the specified base dirs" do
      base <- find (flip isProperPrefixOf path . view #absolute) bases
      rel <- stripProperPrefix (base ^. #absolute) path
      pure (rel, base)
    pure (rel, base)

-- | The base dirs aren't checked for existence since the user might want to create a file in them anyway.
filesMenuConfig ::
  Members [Stop FilesError, Settings, Rpc, Log, Async, Embed IO] r =>
  Path Abs Dir ->
  [Text] ->
  Sem r (
    SerialT IO (MenuItem FileSegments),
    FilesState,
    WindowOptions
  )
filesMenuConfig cwd pathSpecs = do
  conf <- filesConfig
  bufPath <- resolveBufPath baseDirs
  items <- fmap (fmap fileSegments) <$> files conf (view #absolute <$> baseDirs)
  pure (items, FilesState (modal (FilesMode (Filter Fuzzy True) Full)) (Zipper.fromNonEmpty baseDirs) bufPath, window)
  where
    window = def & #prompt . #modes .~ OnlyInsert & #items .~ opt
    opt =
      def {
        name = ScratchId name,
        syntax = [filesSyntax],
        filetype = Just name
      }
    name = "proteome-files"
    baseDirs = fromMaybe [BaseDir cwd (Just [reldir|.|]) True] (nonEmpty specDirs)
    specDirs = absPaths <&> \ p -> BaseDir p (stripProperPrefix cwd p) False
    absPaths = mapMaybe (parsePath cwd) pathSpecs

filesMenu ::
  Members FilesStack r =>
  Members [Stop FilesError, Stop Report, Settings, Rpc] r =>
  Path Abs Dir ->
  [Text] ->
  Sem r ()
filesMenu cwd pathSpecs = do
  mapReport @RpcError do
    (items, s, window) <- filesMenuConfig cwd pathSpecs
    result <- windowMenu items s window actions
    handleResult fileAction result

proFiles ::
  Members FilesStack r =>
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  ArgList ->
  Handler r ()
proFiles (ArgList paths) =
  mapReport @FilesError $ resumeReport @Rpc $ resumeReport @Settings do
    cwd <- resumeHoistAs FilesError.BadCwd nvimCwd
    filesMenu cwd paths
