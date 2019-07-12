module Proteome.Files.Process where

import Conduit (ConduitT, mapC, (.|))
import Control.Concurrent.Lifted (fork)
import Data.Conduit.TMChan (TMChan, newTMChan, sourceTMChan, writeTMChan)
import Path (Abs, Dir, File, Path, stripProperPrefix, toFilePath)
import Path.IO (walkDir)
import qualified Path.IO as WalkAction (WalkAction(WalkExclude))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))

scan ::
  MonadIO m =>
  TMChan [Path Abs File] ->
  Path Abs Dir ->
  m ()
scan chan =
  walkDir enqueue
  where
    enqueue _ _ files' =
      WalkAction.WalkExclude [] <$ atomically (writeTMChan chan files')

formatFileLine :: Path Abs Dir -> Path Abs File -> Text
formatFileLine cwd path =
  toText $ maybe (toFilePath path) toFilePath (stripProperPrefix cwd path)

files ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  NonEmpty (Path Abs Dir) ->
  ConduitT () [MenuItem (Path Abs File)] m ()
files cwd paths = do
  chan <- atomically newTMChan
  lift $ traverse_ (fork . scan chan) paths
  sourceTMChan chan .| mapC (fmap menuItem)
  where
    menuItem path =
      MenuItem path (toText (toFilePath path)) (formatFileLine cwd path)
