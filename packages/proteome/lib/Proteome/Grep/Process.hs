module Proteome.Grep.Process where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as Text
import Data.Text (isInfixOf)
import Path (Abs, File, Path, parseAbsDir, parseAbsFile, toFilePath)
import Path.IO (isLocationOccupied)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Unicode.Stream as Stream
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream, MonadAsync)
import qualified Streamly.System.Process as Process
import Streamly.System.Process (ProcessFailure)

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError (..))
import Proteome.Data.GrepOutputLine (GrepOutputLine)
import Proteome.Grep.Parse (parseGrepOutput)
import Proteome.System.Path (findExe)

patternPlaceholder :: Text
patternPlaceholder =
  "{pattern}"

pathPlaceholder :: Text
pathPlaceholder =
  "{path}"

replaceOrAppend :: Text -> Text -> [Text] -> [Text]
replaceOrAppend placeholder target segments | any (placeholder `isInfixOf`) segments =
  Text.replace placeholder target <$> segments
replaceOrAppend _ target segments =
  segments <> [target]

parseAbsExe ::
  MonadDeepError e GrepError m =>
  Text ->
  m (Path Abs File)
parseAbsExe exe =
  hoistEitherAs (GrepError.NoSuchExecutable exe) $ parseAbsFile (toString exe)

grepCmdline ::
  Text ->
  Text ->
  Text ->
  Text ->
  [Text] ->
  MonadIO m =>
  MonadDeepError e GrepError m =>
  m (Text, [Text])
grepCmdline cmdline patt cwd destination opt = do
  when (Text.null exe) $ throwHoist GrepError.Empty
  absExe <- if absolute exe then parseAbsExe exe else findExe exe
  destPath <- hoistEitherAs destError $ parseAbsDir (toString (absDestination destination))
  unlessM (isLocationOccupied destPath) $ throwHoist destError
  return (toText (toFilePath absExe), withDir destPath)
  where
    absDestination d =
      if d == "."
      then cwd
      else if absolute d
      then d
      else cwd <> "/" <> d
    absolute a =
      Text.take 1 a == "/"
    argSegments =
      opt <> Text.words (Text.strip args)
    (exe, args) =
      Text.breakOn " " (Text.strip cmdline)
    withDir dir =
      replaceOrAppend pathPlaceholder (toText (toFilePath dir)) withPattern
    withPattern =
      replaceOrAppend patternPlaceholder patt argSegments
    destError =
      GrepError.NoSuchDestination destination

processOutput ::
  IsStream t =>
  MonadAsync m =>
  MonadCatch m =>
  Text ->
  [Text] ->
  t m Word8
processOutput exe args =
  Process.toBytes (toString exe) (toString <$> args)

processLines ::
  IsStream t =>
  MonadAsync m =>
  MonadCatch m =>
  Text ->
  [Text] ->
  t m Text
processLines exe args =
  Stream.lines (toText <$> Fold.toList) $
  Stream.decodeUtf8 $
  processOutput exe args

grepMenuItems ::
  MonadRibo m =>
  IsStream t =>
  MonadAsync m =>
  MonadCatch m =>
  Text ->
  Text ->
  [Text] ->
  t m (MenuItem GrepOutputLine)
grepMenuItems cwd exe args =
  Stream.handle processFailure $
  Stream.mapMaybeM (parseGrepOutput cwd) $
  processLines exe args
  where
    processFailure (_ :: ProcessFailure) =
      Stream.nil
