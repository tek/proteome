module Proteome.Test.FilesTest where

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (fromList)
import Path (Abs, Dir, Path, isProperPrefixOf, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assert, assertEq, assertJust, evalMaybe, runTestAuto, unitTest, (===))
import Ribosome (mapReport)
import Ribosome.Api (currentBufferPath, edit)
import Ribosome.Menu (promptInput)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import qualified Ribosome.Settings as Settings
import qualified Streamly.Prelude as Stream
import Test.Tasty (TestTree, testGroup)
import Text.Regex.PCRE.Heavy (re)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import Proteome.Files (filesMenu)
import Proteome.Files.Source (files)
import qualified Proteome.Settings as Settings
import Proteome.Test.Run (proteomeTest)

paths :: Path Abs Dir -> NonEmpty (Path Abs Dir)
paths base =
  (base </> [reldir|dir1|]) :| [base </> [reldir|dir2|]]

editEvents :: [PromptEvent]
editEvents =
  Mapping <$> ["k", "<cr>"]

test_filesEdit :: UnitTest
test_filesEdit =
  proteomeTest do
    Settings.update Settings.filesUseRg False
    dir <- Test.fixturePath [reldir|files|]
    mapReport @FilesError $ promptInput editEvents do
      filesMenu dir (toText . toFilePath <$> NonEmpty.toList (paths dir))
    p <- evalMaybe =<< currentBufferPath
    assert (isProperPrefixOf dir p)

conf :: FilesConfig
conf =
  FilesConfig False True [[re|b/c|]] [[re|/g/|/k/|]] []

test_filesExclude :: UnitTest
test_filesExclude =
  runTestAuto $ asyncToIOFinal do
    dir <- Test.fixturePath [reldir|files|]
    fs <- files conf (paths dir)
    assertEq 3 . length =<< embed (Stream.toList fs)

createEvents :: [PromptEvent]
createEvents =
  [
    Update "p",
    Mapping "<tab>",
    Update "path/t",
    Mapping "<tab>",
    Update "path/to/d",
    Mapping "<tab>",
    Update "path/to/dir/file",
    Mapping "<c-y>"
  ]

test_filesCreate :: UnitTest
test_filesCreate =
  proteomeTest do
    Settings.update Settings.filesUseRg False
    base <- Test.tempDir [reldir|files/create|]
    let targetDir = base </> [reldir|path/to/dir|]
    createDirIfMissing True targetDir
    mapReport @FilesError $ promptInput createEvents do
      filesMenu base [toText (toFilePath base)]
    assertJust (targetDir </> [relfile|file|]) =<< currentBufferPath

createCurDirEvents :: [PromptEvent]
createCurDirEvents =
  [
    Mapping "<c-d>",
    Update "path/to/dir/file",
    Mapping "<c-y>"
  ]

-- TODO this and above need to use MenuTest to directly manipulate the prompt
test_filesCreateCurDir :: UnitTest
test_filesCreateCurDir =
  proteomeTest do
    Settings.update Settings.filesUseRg False
    base <- Test.tempDir baseRel
    let targetDir = base </> createRel
    cur <- Test.tempFile ["cur"] (baseRel </> createRel </> [relfile|cur|])
    createDirIfMissing True targetDir
    edit cur
    mapReport @FilesError $ promptInput createCurDirEvents do
      filesMenu base [toText (toFilePath base)]
    assertJust (targetDir </> [relfile|file|]) =<< currentBufferPath
  where
    createRel = [reldir|path/to/dir|]
    baseRel = [reldir|files/create|]

filesMultiDirTest :: Bool -> UnitTest
filesMultiDirTest rg = do
  runTestAuto $ asyncToIOFinal do
    dir1 <- Test.tempDir sub1
    dir2 <- Test.tempDir sub2
    createDirIfMissing True dir1
    createDirIfMissing True dir2
    Test.tempFile ["content"] (sub1 </> [relfile|file1|])
    Test.tempFile ["content"] (sub2 </> [relfile|file2|])
    Test.tempFile ["content"] (sub1 </> [relfile|file.foo|])
    Test.tempFile ["content"] (sub1 </> [relfile|file.bar|])
    fs <- files conf' (dir1 :| [dir2])
    stream <- fmap MenuItem.render <$> embed (Stream.toList fs)
    target === Set.fromList stream
  where
    sub1 =
      [reldir|files/multi/dir1|]
    sub2 =
      [reldir|files/multi/dir2|]
    conf' =
      FilesConfig rg False [] [] ["*.foo", "bar"]
    target =
      Set.fromList [" * [dir1] file.bar", " * [dir1] file1", " * [dir2] file2"]

test_filesMultiDirNative :: UnitTest
test_filesMultiDirNative =
  filesMultiDirTest False

test_filesMultiDirRg :: UnitTest
test_filesMultiDirRg =
  filesMultiDirTest True

test_files :: TestTree
test_files =
  testGroup "files menu" [
    unitTest "edit a file" test_filesEdit,
    unitTest "exclude patterns" test_filesExclude,
    unitTest "create a file" test_filesCreate,
    unitTest "create a file after inserting the current directory" test_filesCreateCurDir,
    unitTest "show dir prefix, native" test_filesMultiDirNative,
    unitTest "show dir prefix, rg" test_filesMultiDirRg
  ]
