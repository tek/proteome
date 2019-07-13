module Proteome.Files.Process where

import Conduit (ConduitT, mapC, (.|))
import Control.Concurrent.Async.Lifted (async, wait)
import Data.Conduit.TMChan (TMChan, closeTMChan, newTMChan, sourceTMChan, writeTMChan)
import qualified Data.Text as Text (isPrefixOf)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, stripProperPrefix, toFilePath)
import Path.IO (walkDir)
import qualified Path.IO as WalkAction (WalkAction(WalkExclude))
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import Text.RE.PCRE.Text (RE, anyMatches, (*=~))

import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))

matchPath :: [RE] -> Path Abs t -> Bool
matchPath exclude path =
  any (anyMatches . match) exclude
  where
    match regex =
      toText (toFilePath path) *=~ regex

filterPaths ::
  (Path Abs t -> Path Rel t) ->
  Bool ->
  Bool ->
  [RE] ->
  [Path Abs t] ->
  [Path Abs t]
filterPaths lastSegment isInclude excludeHidden patterns =
  filter (negation . matchPath patterns) . filter (not . hidden)
  where
    negation =
      if isInclude then id else not
    hidden =
      if excludeHidden
      then Text.isPrefixOf "." . toText . toFilePath . lastSegment
      else const False

filterFiles ::
  Bool ->
  [RE] ->
  [Path Abs File] ->
  [Path Abs File]
filterFiles =
  filterPaths filename False

filterDirs ::
  Bool ->
  [RE] ->
  [Path Abs Dir] ->
  [Path Abs Dir]
filterDirs =
  filterPaths dirname True

scan ::
  MonadIO m =>
  FilesConfig ->
  TMChan [Path Abs File] ->
  Path Abs Dir ->
  m ()
scan (FilesConfig excludeHidden ignoreFiles ignoreDirs) chan =
  walkDir enqueue
  where
    enqueue _ dirs files' =
      atomically (writeTMChan chan (filterFiles excludeHidden ignoreFiles files')) $>
      WalkAction.WalkExclude (filterDirs excludeHidden ignoreDirs dirs)

runScanners ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  TMChan [Path Abs File] ->
  NonEmpty (Path Abs Dir) ->
  m (NonEmpty ())
runScanners conf chan paths = do
  threads <- traverse (async . scan conf chan) paths
  traverse wait threads <* atomically (closeTMChan chan)

formatFileLine :: Path Abs Dir -> Path Abs File -> Text
formatFileLine cwd path =
  toText $ maybe (toFilePath path) toFilePath (stripProperPrefix cwd path)

files ::
  MonadIO m =>
  MonadBaseControl IO m =>
  FilesConfig ->
  Path Abs Dir ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
files conf cwd paths = do
  chan <- atomically newTMChan
  void . lift . async $ runScanners conf chan paths
  sourceTMChan chan .| mapC (fmap menuItem)
  where
    menuItem path =
      simpleMenuItem path (formatFileLine cwd path)
