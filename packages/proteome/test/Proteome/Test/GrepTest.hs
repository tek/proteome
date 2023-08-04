module Proteome.Test.GrepTest where

import qualified Data.Text.IO as Text
import Path (Abs, File, Path, SomeBase (Abs), absfile, reldir, relfile, toFilePath)
import Path.IO (findExecutable)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, UnitTest, assertEq, unitTest, (===))
import Ribosome (pathText)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu (promptInput)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Prompt (PromptEvent (Mapping))
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.GrepState (GrepOutputLine (GrepOutputLine))
import qualified Proteome.Grep as Grep
import Proteome.Grep (grepWith, uniqueGrepLines)
import Proteome.Grep.Process (grepMenuItems)
import Proteome.Test.Run (proteomeTest)

pat :: Text
pat =
  "target with spaces"

jumpEvents :: [PromptEvent]
jumpEvents =
  Mapping <$> ["k", "<cr>"]

test_grepJump :: UnitTest
test_grepJump =
  proteomeTest do
    dir <- Test.fixturePath [reldir|grep/pro|]
    Grep.handleErrors (promptInput jumpEvents (grepWith [] dir pat))
    assertEq 5 =<< currentLine

yankEvents :: [PromptEvent]
yankEvents =
  Mapping <$> ["k", "y"]

test_grepYank :: UnitTest
test_grepYank = do
  proteomeTest do
    dir <- Test.fixturePath [reldir|grep/pro|]
    Grep.handleErrors (promptInput yankEvents (grepWith [] dir pat))
    normal "P"
    l <- currentBufferContent
    ["line 6 " <> pat, ""] === l

test_grepDuplicates :: UnitTest
test_grepDuplicates =
  proteomeTest do
    rgExe <- stopNote "rg not found" =<< findExecutable [relfile|rg|]
    dir <- Test.fixturePath [reldir|grep/duplicates|]
    let
      proc =
        grepMenuItems dir rgExe ["--vimgrep", "--no-heading", "target", pathText dir]
    outputDupes <- embed . Streamly.toList =<< proc
    2 === length outputDupes
    outputUnique <- embed . Streamly.toList . uniqueGrepLines =<< proc
    1 === length outputUnique
    output3 <- embed (Streamly.toList (uniqueGrepLines (Streamly.fromList (item 1 ++ item 2))))
    1 === length output3
    where
      item col =
        [MenuItem (GrepOutputLine path (Abs path) 0 (Just col) "target" "/path/to/file" "/path/to" "file") "" [""]]
      path = [absfile|/path/to/file|]

deleteEvents :: [PromptEvent]
deleteEvents =
  Mapping <$> ["k", "<space>", "k", "<space>", "d"]

deleteFile1Lines :: [Text]
deleteFile1Lines =
  [
    "target 0",
    "",
    "target 1",
    "",
    "garbage",
    "target 2",
    "target 4",
    "garbage"
  ]

deleteFile1Target :: [Text]
deleteFile1Target =
  [
    "target 0",
    "",
    "garbage",
    "target 2",
    "garbage"
  ]

checkContent ::
  Members [Hedgehog IO, Embed IO] r =>
  Path Abs File ->
  [Text] ->
  Sem r ()
checkContent file target =
  (target ===) . lines . toText =<< embed (Text.readFile (toFilePath file))

deletePattern :: Text
deletePattern = "target"

test_grepDelete :: UnitTest
test_grepDelete =
  proteomeTest do
    dir <- Test.tempDir [reldir|grep/delete|]
    file1 <- Test.tempFile deleteFile1Lines [relfile|grep/delete/file1|]
    Grep.handleErrors (promptInput deleteEvents (grepWith [] dir deletePattern))
    checkContent file1 deleteFile1Target

test_grep :: TestTree
test_grep =
  testGroup "grep" [
    unitTest "jump to a result" test_grepJump,
    unitTest "yank a result" test_grepYank,
    unitTest "filter duplicates" test_grepDuplicates,
    unitTest "delete text in grep results" test_grepDelete
  ]
