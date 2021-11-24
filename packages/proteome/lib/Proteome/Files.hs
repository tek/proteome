module Proteome.Files where

import Control.Lens (_1, use, view)
import Control.Monad (foldM)
import Control.Monad.Catch (MonadCatch)
import Data.Composition ((.:))
import Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import Data.List.NonEmpty.Extra (maximumOn1)
import qualified Data.Sequences as Sequences (filterM)
import qualified Data.Text as Text
import Path (Abs, Dir, File, Path, Rel, parent, parseAbsDir, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesDirExist, listDirRel)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Data.Setting (Setting (Setting))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuOk, menuSuccess, menuUpdatePrompt)
import qualified Ribosome.Menu.Consumer as Consumer
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.MenuConsumer (MenuWidget, MenuWidgetM)
import Ribosome.Menu.Items (withSelectionM)
import Ribosome.Menu.Prompt (defaultPrompt)
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, PromptFlag (OnlyInsert, StartInsert))
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimGetOption)
import Text.RE.PCRE.Text (RE, compileRegex)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import qualified Proteome.Data.FilesError as FilesError (FilesError (..))
import Proteome.Files.Source (files)
import Proteome.Files.Syntax (filesSyntax)
import qualified Proteome.Settings as Settings
import Ribosome.Menu.Data.MenuState (menuRead)

editFile ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuWidget m (Path Abs t) ()
editFile =
  withSelectionM (traverse_ (edit . toFilePath))

matchingDirs ::
  MonadIO m =>
  [Path Abs Dir] ->
  Path Rel Dir ->
  m [Path Abs Dir]
matchingDirs bases path =
  Sequences.filterM doesDirExist ((</> path) <$> bases)

dirsWithPrefix ::
  MonadIO m =>
  Text ->
  Path Abs Dir ->
  m [Path Rel Dir]
dirsWithPrefix (Text.toLower -> prefix) dir =
  filter (Text.isPrefixOf prefix . Text.toLower . toText . toFilePath) . fst <$> listDirRel dir

-- |Search all dirs in @bases@ for relative paths starting with @text@.
-- First, split the last path segment (after /) off and collect the subdirectories of @bases@ that start with the
-- remainder. If there is no / in the text, parsing the remainder fails with 'Nothing' and the @bases@ themselves are
-- used.
-- Then, search the resulting dirs for subdirs starting with the last segment.
-- Return the remainder and the relative subdir paths.
matchingPaths ::
  MonadIO m =>
  [Path Abs Dir] ->
  Text ->
  m (Text, [Path Rel Dir])
matchingPaths bases text' =
  (subpath,) . join <$> (traverse (dirsWithPrefix prefix) =<< dirs)
  where
    subpath =
      maybe "" (toText . toFilePath) dir
    dirs =
      maybe (return bases) (matchingDirs bases) dir
    (dir, prefix) =
      first (parseRelDir . toString) $ Text.breakOnEnd "/" text'

commonPrefix :: [Text] -> Maybe Text
commonPrefix (h : t) =
  foldM (fmap (view _1) .: Text.commonPrefixes) h t
commonPrefix a =
  listToMaybe a

tabComplete ::
  MonadIO m =>
  [Path Abs Dir] ->
  Text ->
  m (Maybe Text)
tabComplete bases promptText = do
  existingBases <- Sequences.filterM doesDirExist bases
  (subpath, paths) <- matchingPaths existingBases promptText
  pure (mappend subpath <$> commonPrefix (toText . toFilePath <$> paths))

tabUpdatePrompt ::
  PromptState ->
  Text ->
  Prompt
tabUpdatePrompt st prefix =
  Prompt (Text.length prefix) st (PromptText prefix)

tab ::
  MonadIO m =>
  [Path Abs Dir] ->
  MenuWidgetM m (Path Abs t) ()
tab bases = do
  Prompt _ promptState (PromptText promptText) <- use Menu.prompt
  lift (tabComplete bases promptText) >>= \case
    Just prefix ->
      menuUpdatePrompt (tabUpdatePrompt promptState prefix)
    Nothing ->
      menuOk

createAndEditFile ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  Path Abs File ->
  m ()
createAndEditFile path =
  tryHoistAnyAs err create *>
  edit (toFilePath path)
  where
    err =
      FilesError.CouldntCreateDir (toText (toFilePath dir))
    create =
      createDirIfMissing True dir
    dir =
      parent path

existingSubdirCount ::
  MonadIO m =>
  [Text] ->
  Path Abs Dir ->
  m Int
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
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  NonEmpty (Path Abs Dir) ->
  MenuWidgetM m (Path Abs t) ()
createFile bases = do
  PromptText promptText <- use (Menu.prompt . Prompt.text)
  menuSuccess do
    let
      parse counts =
        (base counts </>) <$> parseRelFile (toString promptText)
    subdirCounts <- traverse (existingSubdirCount (dirSegments promptText)) bases
    maybe (err promptText) createAndEditFile (parse subdirCounts)
  where
    base counts =
      fst $ maximumOn1 snd (NonEmpty.zip bases counts)
    dirSegments =
      dropEnd 1 . Text.splitOn "/"
    err =
      throwHoist . FilesError.InvalidFilePath

actions ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  NonEmpty (Path Abs Dir) ->
  Map Text (MenuWidget m (Path Abs File) ())
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

readRE ::
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  Text ->
  Text ->
  m RE
readRE name text' =
  maybe (throwHoist (FilesError.BadRE name text')) pure (compileRegex (toString text'))

readREs ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilesError m =>
  MonadDeepError e SettingError m =>
  Setting [Text] ->
  m [RE]
readREs s@(Setting name _ _) =
  traverse (readRE name) =<< setting s

filesConfig ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  m FilesConfig
filesConfig =
  FilesConfig <$> useRg <*> hidden <*> fs <*> dirs <*> wildignore
  where
    useRg =
      setting Settings.filesUseRg
    hidden =
      setting Settings.filesExcludeHidden
    fs =
      readREs Settings.filesExcludeFiles
    dirs =
      readREs Settings.filesExcludeDirectories
    wildignore =
      Text.splitOn "," <$> (vimGetOption "wildignore")

filesWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  PromptConfig m ->
  Path Abs Dir ->
  [Text] ->
  m ()
filesWith promptConfig cwd paths = do
  conf <- filesConfig
  void $ nvimMenu scratchOptions (files conf nePaths) handler promptConfig
  where
    nePaths =
      fromMaybe (cwd :| []) $ nonEmpty absPaths
    absPaths =
      mapMaybe (parsePath cwd) paths
    scratchOptions =
      def {
        _name = name,
        _syntax = [filesSyntax],
        _filetype = Just name
      }
    name =
      "proteome-files"
    handler =
      Consumer.withMappings (actions nePaths)

proFiles ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilesError m =>
  [Text] ->
  m ()
proFiles paths = do
  cwd <- hoistEitherAs FilesError.BadCwd =<< parseAbsDir <$> nvimCwd
  filesWith (defaultPrompt [StartInsert, OnlyInsert]) cwd paths
