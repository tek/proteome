module Proteome.Test.GrepTest where

import Path (absfile, reldir, relfile)
import Path.IO (findExecutable)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, unitTest, (===))
import Ribosome (pathText)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Prompt (promptInputWith)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig))
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import qualified Streamly.Prelude as Stream
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.GrepOutputLine (GrepOutputLine (GrepOutputLine))
import Proteome.Grep (grepWith, uniqueGrepLines)
import Proteome.Grep.Process (grepMenuItems)
import Proteome.Test.Run (proteomeTest)

promptConfig ::
  [Text] ->
  PromptConfig
promptConfig cs =
  PromptConfig (promptInputWith (Just 0.1) Nothing (Stream.fromList cs)) []

pat :: Text
pat =
  "target with spaces"

jumpChars :: [Text]
jumpChars =
  ["k", "cr"]

test_grepJump :: UnitTest
test_grepJump =
  proteomeTest do
    dir <- Test.fixturePath [reldir|grep/pro|]
    grepWith (promptConfig jumpChars) dir pat []
    assertEq 5 =<< currentLine

yankChars :: [Text]
yankChars =
  ["k", "y"]

test_grepYank :: UnitTest
test_grepYank = do
  proteomeTest do
    dir <- Test.fixturePath [reldir|grep/pro|]
    grepWith (promptConfig yankChars) dir pat []
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
        [MenuItem (GrepOutputLine [absfile|/path/to/file|] 0 (Just col) "target") "" ""]

test_noResults :: UnitTest
test_noResults =
  proteomeTest do
    dir <- Test.fixturePath [reldir|grep/pro|]
    grepWith (promptConfig []) dir "nonexistent" []
    l <- currentBufferContent
    [""] === l

test_grep :: TestTree
test_grep =
  testGroup "grep" [
    unitTest "jump to a result" test_grepJump,
    unitTest "yank a result" test_grepYank,
    unitTest "filter duplicates" test_grepDuplicates,
    unitTest "no results" test_noResults
  ]
