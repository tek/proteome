module Proteome.Files where

import Control.Monad (foldM)
import Data.Either.Extra (eitherToMaybe)
import Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import Data.List.NonEmpty.Extra (maximumOn1)
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
  parent,
  parseAbsDir,
  parseRelDir,
  parseSomeFile,
  prjSomeBase,
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
  Filter (Fuzzy),
  Mappings,
  MenuAction,
  MenuWidget,
  Prompt (..),
  PromptConfig (OnlyInsert),
  PromptMode,
  PromptText (PromptText),
  WindowMenus,
  menuOk,
  menuState,
  menuSuccess,
  menuUpdatePrompt,
  modal,
  windowMenu,
  withSelection,
  (%=),
  )
import Ribosome.Menu.Mappings (insert, withInsert)
import Ribosome.Menu.MenuState (mode)
import qualified Ribosome.Settings as Settings
import Text.Regex.PCRE.Light (Regex, compileM)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError (..))
import qualified Proteome.Data.FilesState as FilesState
import Proteome.Data.FilesState (FilesMode (FilesMode), FilesState, Segment (Full), fileSegments)
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

matchingDirs ::
  Member (Embed IO) r =>
  [Path Abs Dir] ->
  Path Rel Dir ->
  Sem r [Path Abs Dir]
matchingDirs bases path =
  filterM (fmap (fromRight False) . tryAny . doesDirExist) ((</> path) <$> bases)

dirsWithPrefix ::
  Member (Embed IO) r =>
  Text ->
  Path Abs Dir ->
  Sem r [Path Rel Dir]
dirsWithPrefix (Text.toLower -> prefix) dir =
  filter (Text.isPrefixOf prefix . Text.toLower . toText . toFilePath) . fst <$> listDirRel dir

-- |Search all dirs in @bases@ for relative paths starting with @text@.
-- First, split the last path segment (after /) off and collect the subdirectories of @bases@ that start with the
-- remainder. If there is no / in the text, parsing the remainder fails with 'Nothing' and the @bases@ themselves are
-- used.
-- Then, search the resulting dirs for subdirs starting with the last segment.
-- Return the remainder and the relative subdir paths.
matchingPaths ::
  Member (Embed IO) r =>
  [Path Abs Dir] ->
  Text ->
  Sem r (Text, [Path Rel Dir])
matchingPaths bases text' =
  (subpath,) . join <$> (traverse (dirsWithPrefix prefix) =<< dirs)
  where
    subpath =
      maybe "" (toText . toFilePath) dir
    dirs =
      maybe (pure bases) (matchingDirs bases) dir
    (dir, prefix) =
      first (parseRelDir . toString) $ Text.breakOnEnd "/" text'

commonPrefix :: [Text] -> Maybe Text
commonPrefix (h : t) =
  foldM (\ p a -> view _1 <$> Text.commonPrefixes p a) h t
commonPrefix a =
  listToMaybe a

tabComplete ::
  Member (Embed IO) r =>
  [Path Abs Dir] ->
  Text ->
  Sem r (Maybe Text)
tabComplete bases promptText = do
  existingBases <- filterM doesDirExist bases
  (subpath, paths) <- matchingPaths existingBases promptText
  pure (mappend subpath <$> commonPrefix (toText . toFilePath <$> paths))

tabUpdatePrompt ::
  PromptMode ->
  Text ->
  Prompt
tabUpdatePrompt st prefix =
  Prompt (Text.length prefix) st (PromptText prefix)

tab ::
  Member (Embed IO) r =>
  [Path Abs Dir] ->
  MenuWidget FilesState r FileAction
tab bases = do
  Prompt _ promptState (PromptText promptText) <- ask
  tabComplete bases promptText >>= \case
    Just prefix ->
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

createFile ::
  Member (Reader Prompt) r =>
  Members [Stop FilesError, Embed IO] r =>
  NonEmpty (Path Abs Dir) ->
  Sem r (Maybe (MenuAction FileAction))
createFile bases = do
  PromptText promptText <- view #text <$> ask
  let
    parse counts =
      parseSomeFile (toString promptText) <&> \case
        Abs p -> p
        Rel p -> base counts </> p
  subdirCounts <- traverse (existingSubdirCount (dirSegments promptText)) bases
  maybe (err promptText) (menuSuccess . Create) (parse subdirCounts)
  where
    base counts =
      fst $ maximumOn1 snd (NonEmpty.zip bases counts)
    dirSegments =
      dropEnd 1 . Text.splitOn "/"
    err =
      stop . FilesError.InvalidFilePath

cycleSegment :: MenuWidget FilesState r FileAction
cycleSegment =
  menuState do
    mode . #segment %= FilesState.cycle
    menuOk

insertFileDir ::
  Member Log r =>
  Maybe (SomeBase File) ->
  MenuWidget FilesState r FileAction
insertFileDir = \case
  Just bufPath -> do
    let dir = prjSomeBase (pathText . parent) bufPath
    Prompt _ m _ <- ask
    menuUpdatePrompt (Prompt (Text.length dir) m (PromptText dir))
  Nothing -> do
    Log.info "Current buffer is not associated with a path"
    menuOk

actions ::
  Members [Stop FilesError, Log, Embed IO] r =>
  NonEmpty (Path Abs Dir) ->
  Maybe (SomeBase File) ->
  Mappings FilesState r FileAction
actions bases bufPath =
  [
    (withInsert "<cr>", editFile),
    (insert "<tab>", tab (NonEmpty.toList bases)),
    (insert "<c-y>", createFile bases),
    (withInsert "<c-s>", cycleSegment),
    (insert "<c-d>", insertFileDir bufPath)
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
    WindowMenus () FilesState !! RpcError,
    Log,
    Async,
    Embed IO
  ]

filesMenu ::
  Members FilesStack r =>
  Members [Stop FilesError, Stop Report, Settings, Rpc] r =>
  Path Abs Dir ->
  [Text] ->
  Sem r ()
filesMenu cwd pathSpecs = do
  mapReport @RpcError do
    conf <- filesConfig
    bufPath <- current
    items <- fmap (fmap fileSegments) <$> files conf nePaths
    result <- windowMenu items (modal (FilesMode Fuzzy Full)) window (actions nePaths bufPath)
    handleResult fileAction result
  where
    window =
      def & #prompt .~ OnlyInsert & #items .~ opt
    opt =
      def {
        name = ScratchId name,
        syntax = [filesSyntax],
        filetype = Just name
      }
    name =
      "proteome-files"
    nePaths =
      fromMaybe (cwd :| []) (nonEmpty absPaths)
    absPaths =
      mapMaybe (parsePath cwd) pathSpecs
    current =
      currentBufferPath <&> fmap \ p ->
        fromMaybe (Abs p) (Rel <$> stripProperPrefix cwd p)

proFiles ::
  Members FilesStack r =>
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  ArgList ->
  Handler r ()
proFiles (ArgList paths) =
  mapReport @FilesError $ resumeReport @Rpc $ resumeReport @Settings do
    cwd <- resumeHoistAs FilesError.BadCwd nvimCwd
    filesMenu cwd paths
