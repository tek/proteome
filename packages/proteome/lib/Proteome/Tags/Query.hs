module Proteome.Tags.Query where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, File, Path)
import Prelude hiding (tag)
import Ribosome (Rpc)
import Ribosome.Api (taglist)
import Ribosome.Menu (MenuItem (MenuItem))

import Proteome.Tags.Mappings (checkPath)
import qualified Proteome.Tags.State as State
import Proteome.Tags.State (
  RawTagSegments (RawTagSegments),
  Tag (Tag),
  TagLoc (TagLoc),
  TagSegments (TagSegments),
  tagLoc,
  )

truncAndPad :: Int -> Text -> Text
truncAndPad n t =
  trunced <> extra
  where
    extra =
      if tooLong then "…" else Text.replicate (diff + 1) " "
    tooLong =
      diff < 0
    diff =
      n - Text.length trunced
    trunced =
      Text.take n t

renderTag :: Tag -> Text
renderTag Tag {path, segments = TagSegments {..}} =
  [exon|#{renderPackage (fromMaybe "local" package)}#{fromMaybe path modulePath}|]
  where
    renderPackage p =
      [exon|📦 #{truncAndPad 20 p} |]

createTag ::
  (RawTagSegments -> TagSegments) ->
  Text ->
  Text ->
  Int ->
  Maybe (MenuItem Tag)
createTag mkSegments name path line = do
  let
    segments = mkSegments (RawTagSegments name path)
    tag = Tag {..}
    rendered = renderTag tag
  pure (MenuItem tag "" [[exon| 🟣 #{truncedName} #{rendered}|]])
  where
    truncedName =
      truncAndPad 20 name

parseTaglistTag ::
  (RawTagSegments -> TagSegments) ->
  TagLoc Text ->
  Maybe (MenuItem Tag)
parseTaglistTag mkSegments loc =
  createTag mkSegments (loc ^. #name) (loc ^. #path) (loc ^. #line)

tagLocs ::
  Member Rpc r =>
  Maybe Text ->
  Maybe (Path Abs File) ->
  Sem r [TagLoc Text]
tagLocs rex file = do
  result <- taglist rex file
  pure (mapMaybe tagLoc result)

checkLocPath ::
  Members [Rpc, Embed IO] r =>
  TagLoc Text ->
  Sem r (Maybe (TagLoc (Path Abs File)))
checkLocPath TagLoc {..} =
  checkPath path <&> fmap \ f -> TagLoc {path = f, ..}

tagLocsPath ::
  Members [Rpc, Embed IO] r =>
  Maybe Text ->
  Maybe (Path Abs File) ->
  Sem r [TagLoc (Path Abs File)]
tagLocsPath rex file =
  mapMaybeM checkLocPath =<< tagLocs rex file

query ::
  Member Rpc r =>
  (RawTagSegments -> TagSegments) ->
  Text ->
  Sem r [MenuItem Tag]
query mkSegments rex =
  mapMaybe (parseTaglistTag mkSegments) <$> tagLocs (Just rex) Nothing
