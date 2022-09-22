module Proteome.Tags.Stream where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Prelude hiding (tag)
import Ribosome (Rpc)
import Ribosome.Api (optionList)
import Ribosome.Menu (MenuItem)
import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Internal.FileSystem.File (toBytes)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Proteome.Tags.Query (createTag)
import Proteome.Tags.State (RawTagSegments, Tag, TagSegments)

parseTagLine ::
  (RawTagSegments -> TagSegments) ->
  Text ->
  Maybe (MenuItem Tag)
parseTagLine mkSegments l =
  case Text.split (== '\t') l of
    [name, path, readMaybe . toString -> Just line] ->
      createTag mkSegments name path line
    _ ->
      Nothing

readLines :: Text -> SerialT IO Text
readLines path =
  Stream.splitOnSuffix (== 10) (decodeUtf8 . ByteString.pack <$> Fold.toList) (toBytes (toString path))

readTags ::
  (RawTagSegments -> TagSegments) ->
  Member Rpc r =>
  Sem r (SerialT IO (MenuItem Tag))
readTags mkSegments = do
  files <- optionList "tags"
  pure (Stream.mapMaybe (parseTagLine mkSegments) (Stream.concatMap readLines (Stream.fromList files)))
