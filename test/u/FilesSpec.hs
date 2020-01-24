{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module FilesSpec (htf_thisModulesTests) where

import Conduit (ConduitT, runConduit, sinkList, yield, yieldMany, (.|))
import Control.Lens (view)
import qualified Data.Conduit.Combinators as Conduit (concat)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (fromList)
import Path (
  Abs,
  Dir,
  Path,
  isProperPrefixOf,
  parseAbsDir,
  parseAbsFile,
  reldir,
  relfile,
  toFilePath,
  (</>),
  )
import Path.IO (createDirIfMissing)
import Ribosome.Api.Buffer (currentBufferName)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (abbreviated)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig), PromptFlag(StartInsert))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Test.Framework
import Text.RE.PCRE.Text (re)

import Proteome.Data.Env (Proteome)
import Proteome.Data.FilesConfig (FilesConfig(FilesConfig))
import Proteome.Files (filesWith)
import Proteome.Files.Source (files)
import Unit (specDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' =
  sleep 0.1 *>
  yieldMany (PromptEvent.Character <$> chars')

slowPromptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
slowPromptInput chars' =
  sleep 0.1 *>
  traverse_ send (PromptEvent.Character <$> chars')
  where
    send c =
      sleep 0.1 *> yield c

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

paths :: Path Abs Dir -> NonEmpty (Path Abs Dir)
paths base =
  (base </> [reldir|dir1|]) :| [base </> [reldir|dir2|]]

editChars :: [Text]
editChars =
  ["k", "k", "k", "cr"]

filesEditSpec :: Proteome ()
filesEditSpec = do
  dir <- parseAbsDir =<< fixture "files"
  filesWith (promptConfig editChars) dir (toText . toFilePath <$> NonEmpty.toList (paths dir))
  gassertEqual True . isProperPrefixOf dir =<< parseAbsFile . toString =<< currentBufferName

test_filesEdit :: IO ()
test_filesEdit =
  specDef filesEditSpec

conf :: FilesConfig
conf =
  FilesConfig True [[re|b/c|]] [[re|/g/|/k/|]]

test_filesExclude :: IO ()
test_filesExclude = do
  dir <- parseAbsDir =<< fixture "files"
  assertEqual 3 . length =<< runConduit (files conf (paths dir) .| Conduit.concat .| sinkList)

createChars :: [Text]
createChars =
  ["p", "tab", "t", "tab", "d", "tab", "f", "i", "l", "e", "c-y"]

filesCreateSpec :: Proteome ()
filesCreateSpec = do
  base <- parseAbsDir =<< tempDir "files/create"
  let targetDir = base </> [reldir|path/to/dir|]
  createDirIfMissing True targetDir
  filesWith slowPromptConfig base [toText (toFilePath base)]
  gassertEqual (targetDir </> [relfile|file|]) =<< parseAbsFile . toString =<< currentBufferName
  where
    slowPromptConfig =
      PromptConfig (slowPromptInput createChars) basicTransition noPromptRenderer [StartInsert]

test_filesCreate :: IO ()
test_filesCreate =
  tmuxSpecDef filesCreateSpec

test_filesMultiDir :: IO ()
test_filesMultiDir = do
  dir1 <- parseAbsDir =<< tempDir "files/multi/dir1"
  dir2 <- parseAbsDir =<< tempDir "files/multi/dir2"
  createDirIfMissing True dir1
  createDirIfMissing True dir2
  writeFile (toFilePath (dir1 </> [relfile|file1|])) "content"
  writeFile (toFilePath (dir2 </> [relfile|file2|])) "content"
  fs <- fmap (view MenuItem.abbreviated) . join <$> runConduit (files conf' (dir1 :| [dir2]) .| sinkList)
  gassertEqual target (Set.fromList fs)
  where
    conf' =
      FilesConfig False [] []
    target =
      Set.fromList [" * [dir1] file1", " * [dir2] file2"]
