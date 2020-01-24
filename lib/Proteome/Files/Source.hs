module Proteome.Files.Source where

import Conduit (ConduitT, mapC, (.|))
import Control.Concurrent.Async.Lifted (async, wait)
import Data.Conduit.TMChan (TMChan, closeTMChan, newTMChan, sourceTMChan, writeTMChan)
import qualified Data.List.NonEmpty as NonEmpty (toList, zip)
import qualified Data.Set as Set (fromList, toList)
import qualified Data.Text as Text (dropEnd, isPrefixOf)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, parent, stripProperPrefix, toFilePath)
import Path.IO (walkDir)
import qualified Path.IO as WalkAction (WalkAction(WalkExclude))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Text.RE.PCRE.Text (RE, anyMatches, (*=~))

import Proteome.Data.FileScanItem (FileScanItem(FileScanItem))
import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))

matchPath :: [RE] -> Path Abs t -> Bool
matchPath exclude path =
  any (anyMatches . match) exclude
  where
    match regex =
      toText (toFilePath path) *=~ regex

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
  [RE] ->
  [Path Abs File] ->
  [Path Abs File]
filterFiles excludeHidden patterns =
    filter (not . matchPath patterns) . filter (not . hiddenFilter filename excludeHidden)

filterDirs ::
  Bool ->
  [RE] ->
  [Path Abs Dir] ->
  [Path Abs Dir]
filterDirs excludeHidden patterns =
  filter pred'
  where
    pred' a =
      matchPath patterns a || hiddenFilter dirname excludeHidden a

scan ::
  MonadIO m =>
  FilesConfig ->
  TMChan [FileScanItem] ->
  Path Abs Dir ->
  Maybe Text ->
  m ()
scan (FilesConfig excludeHidden ignoreFiles ignoreDirs) chan dir baseIndicator =
  walkDir enqueue dir
  where
    enqueue _ dirs files' =
      atomically (writeTMChan chan (cons <$> filterFiles excludeHidden ignoreFiles files')) $>
      WalkAction.WalkExclude (filterDirs excludeHidden ignoreDirs dirs)
    cons =
      FileScanItem dir baseIndicator

runScanners ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  TMChan [FileScanItem] ->
  NonEmpty (Path Abs Dir, Maybe Text) ->
  m (NonEmpty ())
runScanners conf chan paths = do
  threads <- traverse (async . uncurry (scan conf chan)) paths
  traverse wait threads <* atomically (closeTMChan chan)

withBaseIndicators ::
  NonEmpty (Path Abs Dir) ->
  NonEmpty (Path Abs Dir, Maybe Text)
withBaseIndicators bases@(_ :| []) =
  (, Nothing) <$> bases
withBaseIndicators bases =
  NonEmpty.zip bases (findSegment bases)
  where
    findSegment paths =
      if namesUnique paths
      then Just . Text.dropEnd 1 . toText . toFilePath . dirname <$> paths
      else if allEqual next then Nothing <$ paths else findSegment next
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

files ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
files conf paths = do
  chan <- atomically newTMChan
  void . lift . async $ runScanners conf chan (withBaseIndicators paths)
  sourceTMChan chan .| mapC (fmap menuItem)
  where
    menuItem (FileScanItem base baseIndicator path) =
      simpleMenuItem path (formatFileLine base baseIndicator path)
