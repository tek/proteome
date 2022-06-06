module Proteome.Files.Source where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan, writeTMChan)
import Control.Lens (has)
import Control.Lens.Regex.Text (match, regexing)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import qualified Data.Set as Set (fromList, toList)
import qualified Data.Text as Text
import Exon (exon)
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
import Path.IO (doesDirExist, findExecutable, walkDir)
import qualified Path.IO as WalkAction (WalkAction (WalkExclude))
import Ribosome (pathText)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Ribosome.Menu.Stream.Util (chanStream)
import qualified Streamly.Prelude as Streamly
import Streamly.Prelude (SerialT)
import System.FilePattern ((?==))
import Text.Regex.PCRE.Light (Regex)

import Proteome.Data.FileScanItem (FileScanItem (FileScanItem))
import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Grep.Process (processLines)
import Proteome.Path (dropSlash)

-- TODO store traversals instead of Regexes?
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
scan (FilesConfig _ excludeHidden ignoreFiles ignoreDirs wildignore) chan dir baseIndicator =
  tryAny_ do
    walkDir enqueue dir
  where
    enqueue _ dirs files' =
      exclude <$ atomically (traverse_ (writeTMChan chan) filtered)
      where
        filtered =
          cons <$> filterFiles excludeHidden ignoreFiles (toString <$> wildignore) files'
        exclude =
          WalkAction.WalkExclude (filterDirs excludeHidden ignoreDirs dirs)
    cons =
      FileScanItem dir baseIndicator

rgExists ::
  Member (Embed IO) r =>
  Sem r Bool
rgExists =
  isJust <$> findExecutable [relfile|rg|]

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

formatFileLine ::
  Path Abs Dir ->
  Maybe Text ->
  Path Abs File ->
  Text
formatFileLine base baseIndicator path =
  " * " <> maybe "" indicator baseIndicator <> toText (maybe (toFilePath path) toFilePath relativePath)
  where
    relativePath =
      stripProperPrefix base path
    indicator name =
      "[" <> name <> "] "

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
      simpleMenuItem path (formatFileLine base baseIndicator path)

rgExcludes :: FilesConfig -> [Text]
rgExcludes (FilesConfig _ _ _ _ wilds) =
  concat (wild <$> wilds)
  where
    wild i =
      ["--glob", [exon|!#{i}|]]

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
  pure (simpleMenuItem path (formatFileLine base baseIndicator path))

filesRg ::
  Path Abs File ->
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  SerialT IO (MenuItem (Path Abs File))
filesRg rgExe conf paths =
  Streamly.mapMaybe item $
  processLines rgExe ("--files" : excludes <> patterns)
  where
    patterns =
      toText . toFilePath <$> toList paths
    excludes =
      rgExcludes conf
    item =
      rgMenuItem (withBaseIndicators paths)

files ::
  Members [Async, Embed IO] r =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  Sem r (SerialT IO (MenuItem (Path Abs File)))
files conf@(FilesConfig useRg _ _ _ _) paths =
  filterM doesDirExist (toList paths) >>= \case
    [] ->
      pure Streamly.nil
    p : ps ->
      findExecutable [relfile|rg|] >>= \case
        Just rgExe | useRg ->
          pure (filesRg rgExe conf (p :| ps))
        _ ->
          filesNative conf (p :| ps)
