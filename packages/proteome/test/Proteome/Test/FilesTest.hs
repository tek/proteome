module Proteome.Test.FilesTest where

import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (fromList)
import Path (Abs, Dir, Path, isProperPrefixOf, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assert, assertEq, assertJust, evalMaybe, runTestAuto, unitTest, (===))
import Ribosome (mapReport, pathText)
import Ribosome.Api (bufferContent, currentBufferPath, edit)
import Ribosome.Api.Input (feedKey)
import qualified Ribosome.Menu as MenuResult
import Ribosome.Menu (promptInput)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Prompt (PromptEvent (Mapping), updateInsert)
import Ribosome.Menu.Test (confSet, testNativeMenu)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testError)
import Ribosome.Test.Wait ((<--))
import qualified Streamly.Prelude as Stream
import Test.Tasty (TestTree, testGroup)
import Text.Regex.PCRE.Heavy (re)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Data.FilesError (FilesError)
import Proteome.Files (FileAction (Create), actions, fileAction, filesMenu, filesMenuConfig)
import Proteome.Files.Source (files)
import Proteome.Menu (handleResult)
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
conf = FilesConfig False [] True [[re|b/c|]] [[re|/g/|/k/|]] []

test_filesExclude :: UnitTest
test_filesExclude =
  runTestAuto $ asyncToIOFinal do
    dir <- Test.fixturePath [reldir|files|]
    fs <- files conf (paths dir)
    assertEq 3 . length =<< embed (Stream.toList fs)

createEvents :: [PromptEvent]
createEvents =
  [
    updateInsert "p",
    Mapping "<tab>",
    updateInsert "path/t",
    Mapping "<tab>",
    updateInsert "path/to/d",
    Mapping "<tab>",
    updateInsert "path/to/dir/file",
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

test_filesCreateCurDir :: UnitTest
test_filesCreateCurDir =
  proteomeTest $ testError @FilesError do
    Settings.update Settings.filesUseRg False
    base <- Test.tempDir baseRel
    dir1 <- Test.tempDir sub1
    dir2 <- Test.tempDir sub2
    dir3 <- Test.tempDir sub3
    let target = dir3 </> [relfile|sub/file|]
    Test.tempFile ["other"] (sub1 </> [relfile|sub/cur|])
    cur <- Test.tempFile ["cur"] (sub2 </> [relfile|sub/cur|])
    edit cur
    (items, s, window) <- filesMenuConfig base [pathText dir1, pathText dir2, pathText dir3]
    result <- testNativeMenu items (confSet #prompt (window ^. #prompt) def) s (window ^. #items) actions do
      prompt <- MenuUi.promptScratch
      status <- MenuUi.statusScratch
      feedKey "<c-d>"
      ["sub/"] <-- bufferContent (prompt ^. #buffer)
      ["🌳 dir2"] <-- (drop 1 <$> bufferContent (status ^. #buffer))
      traverse_ @[] feedKey ["f", "i", "l", "e"]
      feedKey "<c-b>"
      assertWait (bufferContent (status ^. #buffer)) (assertEq ["🌳 dir3"] . drop 1)
      feedKey "<c-y>"
      MenuTest.result
    MenuResult.Success (Create target) === result
    handleResult fileAction result
    assertJust target =<< currentBufferPath
  where
    sub1 = baseRel </> [reldir|dir1|]
    sub2 = baseRel </> [reldir|dir2|]
    sub3 = baseRel </> [reldir|dir3|]
    baseRel = [reldir|files/create-cur|]

test_filesCreateTab :: UnitTest
test_filesCreateTab =
  proteomeTest $ testError @FilesError do
    Settings.update Settings.filesUseRg False
    base <- Test.tempDir baseRel
    dir1 <- Test.tempDir sub1
    dir2 <- Test.tempDir sub2
    dir3 <- Test.tempDir sub3
    dir4 <- Test.tempDir sub4
    let target = dir4 </> [relfile|second-good/file|]
    Test.tempFile [] (sub1 </> [relfile|first/cur|])
    Test.tempFile [] (sub2 </> [relfile|second-bad/cur|])
    Test.tempFile [] (sub3 </> [relfile|second-good/cur|])
    Test.tempFile [] (sub4 </> [relfile|second-good/cur|])
    (items, s, window) <- filesMenuConfig base [pathText dir1, pathText dir2, pathText dir3, pathText dir4]
    result <- testNativeMenu items (confSet #prompt (window ^. #prompt) def) s (window ^. #items) actions do
      prompt <- MenuUi.promptScratch
      status <- MenuUi.statusScratch
      feedKey "s"
      feedKey "<tab>"
      assertWait (bufferContent (prompt ^. #buffer)) (assertEq ["second-"])
      assertWait (bufferContent (status ^. #buffer)) (assertEq ["🌳 dir2"] . drop 1)
      feedKey "g"
      feedKey "<tab>"
      assertWait (bufferContent (status ^. #buffer)) (assertEq ["🌳 dir3"] . drop 1)
      traverse_ @[] feedKey ["f", "i", "l", "e"]
      feedKey "<c-b>"
      assertWait (bufferContent (status ^. #buffer)) (assertEq ["🌳 dir4"] . drop 1)
      feedKey "<c-y>"
      MenuTest.result
    MenuResult.Success (Create target) === result
  where
    sub1 = baseRel </> [reldir|dir1|]
    sub2 = baseRel </> [reldir|dir2|]
    sub3 = baseRel </> [reldir|dir3|]
    sub4 = baseRel </> [reldir|dir4|]
    baseRel = [reldir|files/create-cur|]

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
    fs <- files conf' [dir1, dir2]
    stream <- fmap (.render) <$> embed (Stream.toList fs)
    target === Set.fromList stream
  where
    sub1 =
      [reldir|files/multi/dir1|]
    sub2 =
      [reldir|files/multi/dir2|]
    conf' =
      FilesConfig rg [] False [] [] ["*.foo", "bar"]
    target =
      Set.fromList [[" * [dir1] file.bar"], [" * [dir1] file1"], [" * [dir2] file2"]]

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
    unitTest "create a file after tabbing" test_filesCreateTab,
    unitTest "show dir prefix, native" test_filesMultiDirNative,
    unitTest "show dir prefix, rg" test_filesMultiDirRg
  ]
