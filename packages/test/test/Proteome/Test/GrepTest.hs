module Proteome.Test.GrepTest where

import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Run (noPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (fixture)
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (serial)
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.GrepOutputLine (GrepOutputLine (GrepOutputLine))
import Proteome.Grep (proGrepWith, uniqueGrepLines)
import Proteome.Grep.Process (grepMenuItems)
import Proteome.Test.Unit (ProteomeTest, testDef, tmuxTest)

promptInput ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInput chars' =
  PromptInput \ _ ->
    serial (Streamly.nilM (sleep 0.1)) (Streamly.fromList (PromptInputEvent.Character <$> chars'))

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

pat :: Text
pat =
  "target with spaces"

jumpChars :: [Text]
jumpChars =
  ["k", "cr"]

grepJumpTest :: ProteomeTest ()
grepJumpTest = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig jumpChars) (toText dir) pat []
  l <- currentLine
  5 === l

test_grepJump :: UnitTest
test_grepJump =
  tmuxTest grepJumpTest

yankChars :: [Text]
yankChars =
  ["k", "y"]

grepYankTest :: ProteomeTest ()
grepYankTest = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig yankChars) (toText dir) pat []
  normal "P"
  l <- currentBufferContent
  ["line 6 " <> pat, ""] === l

test_grepYank :: UnitTest
test_grepYank =
  tmuxTest grepYankTest

grepDuplicatesTest :: ProteomeTest ()
grepDuplicatesTest = do
  dir <- toText <$> fixture "grep/duplicates"
  outputDupes <- Streamly.toList (proc dir)
  2 === length outputDupes
  outputUnique <- Streamly.toList (uniqueGrepLines (proc dir))
  1 === length outputUnique
  output3 <- Streamly.toList (uniqueGrepLines (Streamly.fromList (item 1 ++ item 2)))
  1 === length output3
  where
    proc dir =
      grepMenuItems dir "rg" ["--vimgrep", "--no-heading", "target", dir]
    item col =
      [MenuItem (GrepOutputLine "/path/to/file" 0 (Just col) "target") "" ""]

test_grepDuplicates :: UnitTest
test_grepDuplicates =
  testDef grepDuplicatesTest

test_noResults :: UnitTest
test_noResults =
  testDef @_ @(Ribo _ _) do
    dir <- fixture "grep/pro"
    proGrepWith (promptConfig []) (toText dir) "nonexistent" []
    l <- currentBufferContent
    [] === l

test_grep :: TestTree
test_grep =
  testGroup "grep" [
    unitTest "jump to a result" test_grepJump,
    unitTest "yank a result" test_grepYank,
    unitTest "filter duplicates" test_grepDuplicates,
    unitTest "no results" test_noResults
  ]
