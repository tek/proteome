module Proteome.Data.FilesState where

import qualified Data.List.NonEmpty.Zipper as Zipper
import Data.List.NonEmpty.Zipper (Zipper)
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel, filename, parent)
import Ribosome (pathText)
import Ribosome.Menu (Filter, MenuItem (MenuItem), Modal)
import qualified Ribosome.Menu.MenuState as MenuState
import Ribosome.Menu.MenuState (FilterMode (FilterMode), MenuMode (cycleFilter, renderFilter), MenuState)

data FileSegments =
  FileSegments {
    path :: Path Abs File,
    name :: Text,
    dir :: Text
  }
  deriving stock (Eq, Show, Generic)

fileSegments :: Path Abs File -> FileSegments
fileSegments path =
  FileSegments {
    name = pathText (filename path),
    dir = pathText (parent path),
    ..
  }

data Segment =
  Full
  |
  Name
  |
  Dir
  deriving stock (Eq, Show, Ord)

renderSegment :: Segment -> Text
renderSegment = \case
  Full -> "full"
  Name -> "name"
  Dir -> "dir"

segmentExtract :: MenuItem FileSegments -> Segment -> Text
segmentExtract (MenuItem FileSegments {..} _ render) = \case
  Full -> render
  Name -> name
  Dir -> dir

cycle :: Segment -> Segment
cycle = \case
  Full -> Name
  Name -> Dir
  Dir -> Full

data FilesMode =
  FilesMode {
    mode :: Filter,
    segment :: Segment
  }
  deriving stock (Eq, Show, Ord, Generic)

data BaseDir =
  BaseDir {
    absolute :: Path Abs Dir,
    relative :: Maybe (Path Rel Dir),
    isCwd :: Bool
  }
  deriving stock (Eq, Show, Generic)

data FilesState =
  FilesState {
    modal :: Modal FilesMode FileSegments,
    bases :: Zipper BaseDir,
    bufferPath :: Either Text (Path Rel Dir, BaseDir)
  }
  deriving stock (Show, Generic)

instance MenuMode FileSegments FilesMode where
  type Filter FilesMode =
    FilterMode Filter

  cycleFilter (FilesMode mode segment) =
    FilesMode (cycleFilter mode) segment

  renderFilter (FilesMode mode _) =
    renderFilter mode

  renderExtra (FilesMode _ segment) _ =
    Just [exon|🔧 #{renderSegment segment}|]

  filterMode (FilesMode mode segment) =
    FilterMode mode (Just . flip segmentExtract segment)

cycleBase ::
  Zipper BaseDir ->
  Zipper BaseDir
cycleBase bases =
  fromMaybe (Zipper.start bases) (Zipper.right bases)

instance MenuState FilesState where
  type Item FilesState = FileSegments
  type Mode FilesState = FilesMode

  core = #modal . #core

  mode = #modal . #mode

  histories = #modal . #history

  renderStatus FilesState {bases} space =
    case base of
      Just b -> [[exon|🌳 #{Text.takeEnd (space - 2) (Text.dropWhileEnd ('/' ==) b)}|]]
      Nothing -> []
    where
      base = case Zipper.current bases of
        BaseDir _ _ True -> Nothing
        BaseDir _ (Just relative) _ -> Just (pathText relative)
        BaseDir absolute _ _ -> Just (pathText absolute)
