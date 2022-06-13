module Proteome.Grep.Process where

import qualified Data.Text as Text
import Data.Text (isInfixOf)
import Path (Abs, Dir, File, Path, parseAbsFile, relfile, toFilePath)
import Path.IO (findExecutable, isLocationOccupied)
import Ribosome (pathText)
import Ribosome.Final (inFinal_)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Unicode.Stream as Stream
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)
import qualified Streamly.System.Process as Process
import Streamly.System.Process (ProcessFailure)

import qualified Proteome.Data.GrepError as GrepError (GrepError (..))
import Proteome.Data.GrepError (GrepError)
import Proteome.Data.GrepOutputLine (GrepOutputLine)
import Proteome.Grep.Parse (parseGrepOutput)
import Proteome.System.Path (findExe)

defaultGrepCmdline :: Text
defaultGrepCmdline =
  "grep -Hnr {pattern} {path}"

defaultRgCmdline :: Text
defaultRgCmdline =
  "rg --vimgrep --no-heading -e {pattern} {path}"

defaultCmdline ::
  Member (Embed IO) r =>
  Sem r Text
defaultCmdline =
  findExecutable [relfile|rg|] <&> \case
    Just _ ->
      defaultRgCmdline
    _ ->
      defaultGrepCmdline

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
  Member (Stop GrepError) r =>
  Text ->
  Sem r (Path Abs File)
parseAbsExe exe =
  stopEitherAs (GrepError.NoSuchExecutable exe) $ parseAbsFile (toString exe)

grepCmdline ::
  Members [Stop GrepError, Embed IO] r =>
  Text ->
  Text ->
  Path Abs Dir ->
  [Text] ->
  Sem r (Path Abs File, [Text])
grepCmdline cmdline patt destination opt = do
  when (Text.null exe) do
    stop GrepError.Empty
  absExe <- if absolute exe then parseAbsExe exe else findExe exe
  unlessM (isLocationOccupied destination) do
    stop destError
  pure (absExe, withDir destination)
  where
    absolute a =
      Text.take 1 a == "/"
    argSegments =
      opt <> Text.words (Text.strip args)
    (exe, args) =
      Text.breakOn " " (Text.strip cmdline)
    withDir dir =
      replaceOrAppend pathPlaceholder (pathText dir) withPattern
    withPattern =
      replaceOrAppend patternPlaceholder patt argSegments
    destError =
      GrepError.NoSuchDestination destination

processOutput ::
  IsStream t =>
  String ->
  [Text] ->
  t IO Word8
processOutput exe args =
  Process.toBytes exe (toString <$> args)

processLines ::
  IsStream t =>
  Path Abs File ->
  [Text] ->
  t IO Text
processLines exe args =
  Stream.lines (toText <$> Fold.toList) $
  Stream.decodeUtf8 $
  processOutput (toFilePath exe) args

grepMenuItems ::
  Functor (t IO) =>
  Members [Log, Embed IO, Final IO] r =>
  IsStream t =>
  Path Abs Dir ->
  Path Abs File ->
  [Text] ->
  Sem r (t IO (MenuItem GrepOutputLine))
grepMenuItems cwd exe args =
  inFinal_ \ lower _ pur ->
    pur $
    Stream.handle processFailure $
    Stream.mapMaybeM (fmap join . lower . parseGrepOutput cwd) $
    processLines exe args
  where
    processFailure (_ :: ProcessFailure) =
      Stream.nil
