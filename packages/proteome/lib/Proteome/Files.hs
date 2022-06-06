module Proteome.Files where

import Control.Lens (_1, use, view)
import Control.Monad (foldM)
import Data.Either.Extra (eitherToMaybe)
import Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import Data.List.NonEmpty.Extra (maximumOn1)
import qualified Data.Sequences as Sequences (filterM)
import qualified Data.Text as Text
import Path (Abs, Dir, File, Path, Rel, parent, parseAbsDir, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesDirExist, listDirRel)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  Handler,
  HandlerError,
  Rpc,
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  mapHandlerError,
  )
import Ribosome.Api (nvimGetOption)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.ScratchOptions (ScratchOptions (filetype, name, syntax))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Errors (pluginHandlerErrors)
import Ribosome.Menu (
  MenuAction,
  MenuSem,
  MenuWidget,
  MenuWrite,
  Prompt (..),
  PromptConfig,
  PromptFlag (OnlyInsert, StartInsert),
  PromptListening,
  PromptMode,
  PromptText (PromptText),
  defaultPrompt,
  interpretMenu,
  menuOk,
  menuRead,
  menuSuccess,
  menuUpdatePrompt,
  nvimMenuDef,
  semState,
  withMappings,
  withSelection,
  )
import qualified Ribosome.Settings as Settings
import Text.Regex.PCRE.Light (Regex, compileM)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError (..))
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
  MenuWrite (Path Abs File) r =>
  MenuWidget r FileAction
editFile =
  withSelection (pure . Edit)

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
  existingBases <- Sequences.filterM doesDirExist bases
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
  MenuSem (Path Abs File) r (Maybe (MenuAction FileAction))
tab bases = do
  Prompt _ promptState (PromptText promptText) <- semState (use #prompt)
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
  Members [Stop FilesError, Embed IO] r =>
  NonEmpty (Path Abs Dir) ->
  MenuSem (Path Abs File) r (Maybe (MenuAction FileAction))
createFile bases = do
  PromptText promptText <- semState (use (#prompt . #text))
  let
    parse counts =
      (base counts </>) <$> parseRelFile (toString promptText)
  subdirCounts <- traverse (existingSubdirCount (dirSegments promptText)) bases
  maybe (err promptText) (menuSuccess . Create) (parse subdirCounts)
  where
    base counts =
      fst $ maximumOn1 snd (NonEmpty.zip bases counts)
    dirSegments =
      dropEnd 1 . Text.splitOn "/"
    err =
      stop . FilesError.InvalidFilePath

actions ::
  Members [Stop FilesError, Embed IO] r =>
  MenuWrite (Path Abs File) r =>
  NonEmpty (Path Abs Dir) ->
  Map Text (MenuWidget r FileAction)
actions bases =
  [
    ("cr", editFile),
    ("tab", menuRead (tab (NonEmpty.toList bases))),
    ("c-y", menuRead (createFile bases))
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
  Members [Rpc, Stop FilesError, Stop HandlerError, Embed IO] r =>
  FileAction ->
  Sem r ()
fileAction = \case
  Create path ->
    createAndEditFile path
  Edit paths ->
    traverse_ edit paths
  NoAction ->
    unit

filesWith ::
  MenuWrite (Path Abs File) r =>
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Async, Embed IO, Final IO] r =>
  Members [Stop FilesError, Stop HandlerError, Settings, Settings !! SettingError, Scratch, Rpc, Rpc !! RpcError] r =>
  PromptConfig ->
  Path Abs Dir ->
  [Text] ->
  Sem r ()
filesWith promptConfig cwd paths =
  withMappings (actions nePaths) do
    conf <- filesConfig
    items <- files conf nePaths
    result <- nvimMenuDef scratchOptions items promptConfig
    handleResult "files" fileAction result
  where
    nePaths =
      fromMaybe (cwd :| []) $ nonEmpty absPaths
    absPaths =
      mapMaybe (parsePath cwd) paths
    scratchOptions =
      def {
        name = ScratchId name,
        syntax = [filesSyntax],
        filetype = Just name
      }
    name =
      "proteome-files"

proFiles ::
  Member ChronosTime r =>
  Members [Scratch !! RpcError, Settings !! SettingError, Rpc !! RpcError] r =>
  Members [Sync PromptListening, Log, Mask res, Race, Resource, Async, Embed IO, Final IO] r =>
  [Text] ->
  Handler r ()
proFiles paths =
  mapHandlerError @FilesError $ pluginHandlerErrors $ interpretMenu do
    cwd <- resumeHoistAs FilesError.BadCwd nvimCwd
    conf <- defaultPrompt [StartInsert, OnlyInsert]
    filesWith conf cwd paths
