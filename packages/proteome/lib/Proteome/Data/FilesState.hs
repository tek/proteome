module Proteome.Data.FilesState where

import Path (Abs, File, Path, filename, parent)
import Ribosome (pathText)
import Ribosome.Menu (Filter, MenuItem (MenuItem), Modal)
import qualified Ribosome.Menu.MenuState as MenuState
import Ribosome.Menu.MenuState (FilterMode (FilterMode), MenuMode (cycleFilter, renderFilter))
import Exon (exon)

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

type FilesState =
  Modal FilesMode FileSegments

instance MenuMode FileSegments FilesMode where
  type Filter FilesMode =
    FilterMode Filter

  cycleFilter (FilesMode mode segment) =
    FilesMode (cycleFilter mode) segment

  renderFilter (FilesMode mode _) =
    renderFilter mode

  renderExtra (FilesMode _ segment) =
    Just [exon|🔧 #{renderSegment segment}|]

  filterMode (FilesMode mode segment) =
    FilterMode mode (flip segmentExtract segment)
