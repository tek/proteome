module Proteome.Data.GrepState where

import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), filename, parent, stripProperPrefix)
import Ribosome (MsgpackEncode, pathText)
import Ribosome.Menu (Filter, MenuItem (MenuItem), Modal)
import qualified Ribosome.Menu.MenuState as MenuState
import Ribosome.Menu.MenuState (MenuMode (cycleFilter, matcher, renderFilter))

data GrepOutputLine =
  GrepOutputLine {
    file :: Path Abs File,
    relative :: SomeBase File,
    line :: Int,
    col :: Maybe Int,
    content :: Text,
    path :: Text,
    name :: Text,
    dir :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode)

sameLine :: GrepOutputLine -> GrepOutputLine -> Bool
sameLine l r =
  l.file == r.file && l.line == r.line

grepOutputLine ::
  Path Abs Dir ->
  Path Abs File ->
  Int ->
  Maybe Int ->
  Text ->
  GrepOutputLine
grepOutputLine cwd file line col content =
  GrepOutputLine {
    path = pathText file,
    relative = maybe (Abs file) Rel (stripProperPrefix cwd file),
    name = pathText (filename file),
    dir = pathText (parent file),
    ..
  }

data Segment =
  Full
  |
  Content
  |
  Path
  |
  Name
  |
  Dir
  deriving stock (Eq, Show, Ord)

renderSegment :: Segment -> Text
renderSegment = \case
  Full -> "full"
  Content -> "content"
  Path -> "path"
  Name -> "name"
  Dir -> "dir"

segmentExtract :: MenuItem GrepOutputLine -> Segment -> Text
segmentExtract (MenuItem GrepOutputLine {..} _ render) = \case
  Full -> Text.unlines (toList render)
  Content -> content
  Path -> path
  Name -> name
  Dir -> dir

cycle :: Segment -> Segment
cycle = \case
  Full -> Content
  Content -> Path
  Path -> Name
  Name -> Dir
  Dir -> Full

data GrepMode =
  GrepMode {
    mode :: Filter,
    segment :: Segment
  }
  deriving stock (Eq, Show, Ord, Generic)

type GrepState =
  Modal GrepMode GrepOutputLine

instance MenuMode GrepOutputLine GrepMode where

  cycleFilter (GrepMode mode segment) =
    GrepMode (cycleFilter mode) segment

  renderFilter (GrepMode mode _) =
    renderFilter mode

  renderExtra (GrepMode _ segment) _ =
    Just [exon|🔧 #{renderSegment segment}|]

  matcher (GrepMode mode _) = matcher mode

  extract mode item = Just (segmentExtract item mode.segment)
