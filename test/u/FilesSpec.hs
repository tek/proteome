{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module FilesSpec (htf_thisModulesTests) where

import Conduit (ConduitT, runConduit, sinkList, yield, yieldMany, (.|))
import qualified Data.Conduit.Combinators as Conduit (concat)
import qualified Data.List.NonEmpty as NonEmpty (toList)
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
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
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
  PromptConfig (promptInput cs) basicTransition noPromptRenderer False

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
  assertEqual 3 . length =<< runConduit (files conf dir (paths dir) .| Conduit.concat .| sinkList)

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
      PromptConfig (slowPromptInput createChars) basicTransition noPromptRenderer True

test_filesCreate :: IO ()
test_filesCreate =
  tmuxSpecDef filesCreateSpec
