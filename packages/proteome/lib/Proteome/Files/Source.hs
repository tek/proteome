module Proteome.Files.Source where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan, readTMChan, writeTMChan)
import qualified Control.Exception as Base
import Control.Lens.Regex.Text (match, regexing)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import qualified Data.Set as Set (fromList, toList)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  dirname,
  filename,
  isProperPrefixOf,
  parent,
  parseAbsFile,
  relfile,
  stripProperPrefix,
  toFilePath,
  )
import Path.IO (doesDirExist, findExecutable, getTempDir, openTempFile, removeFile, walkDir)
import qualified Path.IO as WalkAction (WalkAction (WalkExclude))
import Ribosome (pathText)
import Ribosome.Menu (MenuItem (MenuItem))
import Ribosome.Menu.Stream.Util (takeUntilNothing)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream, SerialT)
import System.FilePattern ((?==))
import System.IO (Handle, hClose)
import System.IO.Error (IOError)
import Text.Regex.PCRE.Light (Regex)

import Proteome.Data.FileScanItem (FileScanItem (FileScanItem))
import qualified Proteome.Data.FilesConfig
import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Grep.Process (processLines)
import Proteome.Path (dropSlash)

matchPath :: [Regex] -> Path Abs t -> Bool
matchPath excludes path =
  any check excludes
  where
    check rgx =
      pathText path & has (regexing rgx . match)

hiddenFilter ::
  (Path Abs t -> Path Rel t) ->
  Bool ->
  Path Abs t ->
  Bool
hiddenFilter lastSegment True =
  Text.isPrefixOf "." . toText . toFilePath . lastSegment
hiddenFilter _ False =
  const False

filterFiles ::
  Bool ->
  [Regex] ->
  [String] ->
  [Path Abs File] ->
  [Path Abs File]
filterFiles excludeHidden patterns wildignore =
  filter (not . cond)
  where
    cond path =
      matchPath patterns path ||
      hiddenFilter filename excludeHidden path ||
      any (?== name) wildignore
      where
        name =
          toFilePath (filename path)

filterDirs ::
  Bool ->
  [Regex] ->
  [Path Abs Dir] ->
  [Path Abs Dir]
filterDirs excludeHidden patterns =
  filter pred'
  where
    pred' a =
      matchPath patterns a || hiddenFilter dirname excludeHidden a

scan ::
  Member (Embed IO) r =>
  FilesConfig ->
  TMChan FileScanItem ->
  Path Abs Dir ->
  Maybe Text ->
  Sem r ()
scan FilesConfig {ignoreHidden, ignoreFiles, ignoreDirs, wildignore} chan dir baseIndicator =
  tryAny_ do
    walkDir enqueue dir
  where
    enqueue _ dirs files' =
      exclude <$ atomically (traverse_ (writeTMChan chan) filteredFiles)
      where
        filteredFiles =
          cons <$> filterFiles ignoreHidden ignoreFiles (toString <$> wildignore) files'
        exclude =
          WalkAction.WalkExclude (filterDirs ignoreHidden ignoreDirs dirs)
    cons =
      FileScanItem dir baseIndicator

runScanners ::
  Members [Async, Embed IO] r =>
  FilesConfig ->
  TMChan FileScanItem ->
  NonEmpty (Path Abs Dir, Maybe Text) ->
  Sem r ()
runScanners conf chan paths = do
  threads <- traverse (async . uncurry (scan conf chan)) paths
  traverse_ await threads <* embed (atomically (closeTMChan chan))

withBaseIndicators ::
  NonEmpty (Path Abs Dir) ->
  NonEmpty (Path Abs Dir, Maybe Text)
withBaseIndicators bases@(_ :| []) =
  (, Nothing) <$> bases
withBaseIndicators bases =
  NonEmpty.zip bases (findSegment bases)
  where
    findSegment paths
      | namesUnique paths = Just . dropSlash . dirname <$> paths
      | allEqual next = Nothing <$ paths
      | otherwise = findSegment next
      where
          next = parent <$> paths
    namesUnique paths =
      uniq names == NonEmpty.toList names
      where
        names = dirname <$> paths
    allEqual paths =
      length (uniq paths) == 1
    uniq as =
      Set.toList (Set.fromList (NonEmpty.toList as))

fileMenuItem ::
  Path Abs Dir ->
  Maybe Text ->
  Path Abs File ->
  MenuItem (Path Abs File)
fileMenuItem base baseIndicator path =
  MenuItem path text [display]
  where
    display =
      " * " <> maybe "" indicator baseIndicator <> text
    text =
      toText (maybe (toFilePath path) toFilePath relativePath)
    relativePath =
      stripProperPrefix base path
    indicator name =
      "[" <> name <> "] "

chanStream ::
  IsStream t =>
  Functor (t IO) =>
  TMChan a ->
  t IO a
chanStream chan =
  takeUntilNothing (Stream.repeatM (liftIO (atomically (readTMChan chan))))

filesNative ::
  Members [Async, Embed IO] r =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  Sem r (SerialT IO (MenuItem (Path Abs File)))
filesNative conf paths = do
  chan <- embed (atomically newTMChan)
  void . async $ runScanners conf chan (withBaseIndicators paths)
  pure (menuItem <$> chanStream chan)
  where
    menuItem (FileScanItem base baseIndicator path) =
      fileMenuItem base baseIndicator path

rgExcludes :: FilesConfig -> [Text]
rgExcludes FilesConfig {rgExclude, wildignore} =
  rgExclude ++ wildignore

findBase ::
  Path Abs File ->
  NonEmpty (Path Abs Dir, Maybe Text) ->
  Maybe (Path Abs Dir, Maybe Text)
findBase file =
  find ((`isProperPrefixOf` file) . fst)

rgMenuItem ::
  NonEmpty (Path Abs Dir, Maybe Text) ->
  Text ->
  Maybe (MenuItem (Path Abs File))
rgMenuItem bases file = do
  path <- parseAbsFile (toString file)
  (base, baseIndicator) <- findBase path bases
  pure (fileMenuItem base baseIndicator path)

filesRgStream ::
  Path Abs File ->
  NonEmpty (Path Abs Dir) ->
  [Text] ->
  Maybe (Path Abs File, Handle) ->
  SerialT IO (MenuItem (Path Abs File))
filesRgStream rgExe paths excludes ignoreFile = do
  for_ ignoreFile \ (_, handle) ->
    Stream.fromEffect $ Base.try @IOError do
      Text.hPutStrLn handle (Text.unlines excludes)
      hClose handle
  Stream.mapMaybe item (processLines rgExe args)
  where
    item = rgMenuItem (withBaseIndicators paths)

    args = "--files" : foldMap ignoreOption ignoreFile <> patterns

    ignoreOption (path, _) = ["--ignore-file", pathText path]

    patterns = toText . toFilePath <$> toList paths

rgIgnoreTempFile ::
  IsStream t =>
  (Maybe (Path Abs File, Handle) -> t IO a) ->
  t IO a
rgIgnoreTempFile =
  Stream.bracket acquireTemp releaseTemp
  where
    acquireTemp = do
      tmpfile <- Base.try @IOError do
        tmp <- getTempDir
        openTempFile tmp "proteome-rg-ignore.conf"
      -- Silently discarding temp access errors for now
      pure (rightToMaybe tmpfile)

    releaseTemp =
      traverse_ \ (path, handle) -> do
        void (Base.try @IOError (hClose handle))
        void (Base.try @IOError (removeFile path))

filesRg ::
  Path Abs File ->
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  SerialT IO (MenuItem (Path Abs File))
filesRg rgExe conf paths =
  bracketExcludes (filesRgStream rgExe paths excludes)
  where
    bracketExcludes | null excludes = \ f -> f Nothing
                    | otherwise = rgIgnoreTempFile
    excludes = rgExcludes conf

files ::
  Members [Async, Embed IO] r =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  Sem r (SerialT IO (MenuItem (Path Abs File)))
files conf@FilesConfig {useRg} paths =
  filterM doesDirExist (toList paths) >>= \case
    [] ->
      pure Stream.nil
    p : ps ->
      findExecutable [relfile|rg|] >>= \case
        Just rgExe | useRg ->
          pure (filesRg rgExe conf (p :| ps))
        _ ->
          filesNative conf (p :| ps)
