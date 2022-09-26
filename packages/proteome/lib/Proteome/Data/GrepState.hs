module Proteome.Data.GrepState where

import Exon (exon)
import Path (Abs, File, Path, filename, parent)
import Ribosome (MsgpackEncode, pathText)
import Ribosome.Menu (Filter, MenuItem (MenuItem), Modal)
import qualified Ribosome.Menu.MenuState as MenuState
import Ribosome.Menu.MenuState (FilterMode (FilterMode), MenuMode (cycleFilter, renderFilter))

data GrepOutputLine =
  GrepOutputLine {
    path :: Path Abs File,
    line :: Int,
    col :: Maybe Int,
    content :: Text,
    name :: Text,
    dir :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode)

sameLine :: GrepOutputLine -> GrepOutputLine -> Bool
sameLine (GrepOutputLine p1 l1 _ _ _ _) (GrepOutputLine p2 l2 _ _ _ _) =
  p1 == p2 && l1 == l2

grepOutputLine ::
  Path Abs File ->
  Int ->
  Maybe Int ->
  Text ->
  GrepOutputLine
grepOutputLine path line col content =
  GrepOutputLine {
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
  |
  Content
  deriving stock (Eq, Show, Ord)

renderSegment :: Segment -> Text
renderSegment = \case
  Full -> "full"
  Name -> "name"
  Dir -> "dir"
  Content -> "content"

segmentExtract :: MenuItem GrepOutputLine -> Segment -> Text
segmentExtract (MenuItem GrepOutputLine {..} _ render) = \case
  Full -> render
  Name -> name
  Dir -> dir
  Content -> content

cycle :: Segment -> Segment
cycle = \case
  Full -> Name
  Name -> Dir
  Dir -> Content
  Content -> Full

data FilesMode =
  FilesMode {
    mode :: Filter,
    segment :: Segment
  }
  deriving stock (Eq, Show, Ord, Generic)

type FilesState =
  Modal FilesMode GrepOutputLine

instance MenuMode GrepOutputLine FilesMode where
  type Filter FilesMode =
    FilterMode Filter

  cycleFilter (FilesMode mode segment) =
    FilesMode (cycleFilter mode) segment

  renderFilter (FilesMode mode _) =
    renderFilter mode

  renderExtra (FilesMode _ segment) =
    Just [exon|🔧 #{renderSegment segment}|]

  filterMode (FilesMode mode segment) =
    FilterMode mode (Just . flip segmentExtract segment)
