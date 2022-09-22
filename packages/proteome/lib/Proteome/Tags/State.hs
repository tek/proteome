module Proteome.Tags.State where

import Control.Lens.Regex.Text (Match, group, regex)
import Data.Char (isUpper)
import Data.List.Extra (takeWhileEnd)
import qualified Data.Text as Text
import Exon (exon)
import Lens.Micro.Extras (preview, view)
import Prelude hiding (group)
import qualified Ribosome
import Ribosome.Menu (Filter, MenuItem, Modal)
import qualified Ribosome.Menu.MenuState as MenuState
import Ribosome.Menu.MenuState (FilterMode (FilterMode), MenuMode (cycleFilter, filterMode, renderExtra, renderFilter))

import Proteome.Data.ProjectType (ProjectType (ProjectType))

data TagLoc p =
  TagLoc {
    name :: Text,
    path :: p,
    line :: Int
  }
  deriving stock (Eq, Show, Generic)

tagLoc :: Ribosome.Tag -> Maybe (TagLoc Text)
tagLoc t = do
  line <- readMaybe (toString (t ^. #cmd))
  pure TagLoc {name = t ^. #name, path = t ^. #filename, line = line - 1}

data RawTagSegments =
  RawTagSegments {
    name :: Text,
    path :: Text
  }
  deriving stock (Eq, Show, Generic)

data TagSegments =
  TagSegments {
    name :: Text,
    package :: Maybe Text,
    modulePath :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data Tag =
  Tag {
    path :: Text,
    line :: Int,
    segments :: TagSegments
  }
  deriving stock (Eq, Show, Generic)

data Segment =
  Name
  |
  Package
  |
  Module
  deriving stock (Eq, Show, Ord)

genericSegments :: RawTagSegments -> TagSegments
genericSegments RawTagSegments {..} =
  TagSegments {package = Nothing, modulePath = Nothing, ..}

nixPackageRegex :: Traversal' Text Match
nixPackageRegex =
  [regex|/nix/store/[^-]+-([^/]+?)(-[\d.]+)?(-tags)?/|]

nixPackage :: Text -> Maybe Text
nixPackage =
  preview (nixPackageRegex . group 0)

haskellModule :: Text -> Maybe Text
haskellModule =
  Just .
  Text.dropEnd 3 .
  Text.intercalate "." .
  takeWhileEnd firstUpper .
  Text.split ('/' ==)
  where
    firstUpper seg =
      any (isUpper . fst) (Text.uncons seg)

haskellSegments :: RawTagSegments -> TagSegments
haskellSegments RawTagSegments {..} =
  TagSegments {package = nixPackage path, modulePath = haskellModule path, ..}

tagSegmentsForProject :: ProjectType -> RawTagSegments -> TagSegments
tagSegmentsForProject = \case
  ProjectType "haskell" ->
    haskellSegments
  _ ->
    genericSegments

tagSegmentsForFile :: RawTagSegments -> TagSegments
tagSegmentsForFile segs =
  case Text.takeWhileEnd (/= '.') (segs ^. #path) of
    "hs" ->
      haskellSegments segs
    _ ->
      genericSegments segs

tagSegmentsFor :: Maybe ProjectType -> RawTagSegments -> TagSegments
tagSegmentsFor =
  maybe tagSegmentsForFile tagSegmentsForProject

renderSegment :: Segment -> Text
renderSegment = \case
  Name -> "name"
  Package -> "package"
  Module -> "module"

segmentExtract :: MenuItem Tag -> Segment -> Maybe Text
segmentExtract (view #meta -> Tag {segments = TagSegments {..}}) = \case
  Name -> Just name
  Package -> package
  Module -> modulePath

cycle :: Segment -> Segment
cycle = \case
  Name -> Package
  Package -> Module
  Module -> Name

data TagsMode =
  TagsMode {
    mode :: Filter,
    segment :: Segment
  }
  deriving stock (Eq, Show, Ord, Generic)

type TagsState =
  Modal TagsMode Tag

instance MenuMode Tag TagsMode where
  type Filter TagsMode =
    FilterMode Filter

  cycleFilter (TagsMode mode segment) =
    TagsMode (cycleFilter mode) segment

  renderFilter (TagsMode mode _) =
    renderFilter mode

  renderExtra (TagsMode _ segment) =
    Just [exon|🔧 #{renderSegment segment}|]

  filterMode (TagsMode mode segment) =
    FilterMode mode (flip segmentExtract segment)
