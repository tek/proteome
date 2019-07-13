{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module FilesSpec (htf_thisModulesTests) where

import Conduit (ConduitT, runConduit, sinkList, yieldMany, (.|))
import qualified Data.Conduit.Combinators as Conduit (concat)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Path (Abs, Dir, Path, isProperPrefixOf, parseAbsDir, parseAbsFile, reldir, toFilePath, (</>))
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
import Proteome.Files.Process (files)
import Unit (specDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' =
  sleep 0.1 *>
  yieldMany (PromptEvent.Character <$> chars')

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
