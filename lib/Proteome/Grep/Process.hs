module Proteome.Grep.Process where

import Conduit (ConduitT, await, mapC, yield, (.|))
import Control.Monad.Catch (MonadThrow)
import Data.Composition ((.:))
import qualified Data.Conduit.Combinators as Conduit (chunksOfE, decodeUtf8, linesUnbounded)
import qualified Data.Conduit.List as Conduit (mapMaybeM)
import Data.Conduit.Process.Typed (createSource)
import qualified Data.Set as Set (difference, empty, fromList, toList, union)
import Data.Text (isInfixOf)
import qualified Data.Text as Text (breakOn, null, replace, splitOn, strip, take)
import Path (Abs, File, Path, parseAbsFile, toFilePath)
import Path.IO (isLocationOccupied)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import System.Process.Typed (
  Process,
  getStdout,
  proc,
  setStdout,
  startProcess,
  )

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError(..))
import Proteome.Data.GrepOutputLine (GrepOutputLine)
import Proteome.Grep.Parse (parseGrepOutput)
import Proteome.System.Path (findExe)

patternPlaceholder :: Text
patternPlaceholder =
  "${pattern}"

pathPlaceholder :: Text
pathPlaceholder =
  "${path}"

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
  MonadIO m =>
  MonadDeepError e GrepError m =>
  m (Text, [Text])
grepCmdline cmdline patt cwd destination = do
  when (Text.null exe) $ throwHoist GrepError.Empty
  absExe <- if absolute exe then parseAbsExe exe else findExe exe
  destPath <- hoistEitherAs destError $ parseAbsFile (toString (absDestination destination))
  unlessM (isLocationOccupied destPath) $ throwHoist destError
  return (toText (toFilePath absExe), withDir destPath)
  where
    absDestination d =
      if absolute d then d else cwd <> "/" <> d
    absolute a =
      Text.take 1 a == "/"
    argSegments =
      Text.splitOn " " (Text.strip args)
    (exe, args) =
      Text.breakOn " " (Text.strip cmdline)
    withDir dir =
      replaceOrAppend pathPlaceholder (toText (toFilePath dir)) withPattern
    withPattern =
      replaceOrAppend patternPlaceholder patt argSegments
    destError =
      GrepError.NoSuchDestination destination

grepProcess ::
  MonadIO m =>
  Text ->
  [Text] ->
  m (Process () (ConduitT () ByteString m ()) ())
grepProcess exe args =
  startProcess (config (toString exe) (toString <$> args))
  where
    config =
      setStdout createSource .: proc

grep ::
  MonadRibo m =>
  MonadThrow m =>
  Text ->
  Text ->
  [Text] ->
  ConduitT () [MenuItem GrepOutputLine] m ()
grep cwd exe args = do
  prc <- lift $ grepProcess exe args
  getStdout prc .| Conduit.decodeUtf8 .| Conduit.linesUnbounded .| parse .| mapC pure .| Conduit.chunksOfE 20
  where
    parse =
      Conduit.mapMaybeM (parseGrepOutput cwd)

unique ::
  Show a =>
  Ord a =>
  Monad m =>
  ConduitT [a] [a] m ()
unique =
  consume Set.empty
  where
    consume seen =
      traverse_ (rec seen) =<< await
    rec seen as =
      yield (Set.toList uniques) *>
      consume (Set.union uniques seen)
      where
        uniques =
          Set.difference (Set.fromList as) seen
