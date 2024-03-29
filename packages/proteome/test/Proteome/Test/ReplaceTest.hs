module Proteome.Test.ReplaceTest where

import Control.Lens.Regex.Text (group, regex)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Path (Abs, File, Path, reldir, relfile, toFilePath)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, UnitTest, assertEq, (===))
import Prelude hiding (group)
import Ribosome.Api (nvimGetCurrentWin, nvimWinGetHeight, vimGetBuffers)
import Ribosome.Api.Buffer (buflisted, currentBufferContent, setCurrentBufferContent)
import Ribosome.Menu (promptInput)
import Ribosome.Menu.Prompt (PromptEvent (Mapping))

import qualified Proteome.Grep as Grep
import Proteome.Grep (grepWith)
import Proteome.Grep.Replace (proReplaceQuit, proReplaceSave)
import Proteome.Test.Run (proteomeTest)

pat :: Text
pat = "target"

replacePat :: Text
replacePat = "target!"

replacement :: Text
replacement = "replaced"

replaceEvents :: [PromptEvent]
replaceEvents =
  Mapping <$> ["*", "r"]

file1Lines :: [Text]
file1Lines =
  [
    "garbage 0",
    "target!",
    "garbage 1",
    "and target! here"
  ]

file2Lines :: [Text]
file2Lines =
  [
    "garbage 2",
    "and target!"
  ]

file3Lines :: [Text]
file3Lines =
  [
    "delete me target! 1",
    "",
    "keep me 1",
    "",
    "delete me target! 2",
    "",
    "keep me target 2",
    "delete me target! 3",
    "keep me target 3",
    "",
    "keep me 4",
    "delete me target! 4",
    ""
  ]

file1Target :: [Text]
file1Target =
  [
    "garbage 0",
    "replaced",
    "garbage 1",
    "and replaced here"
  ]

file2Target :: [Text]
file2Target =
  [
    "garbage 2",
    "and replaced"
  ]

file3Target :: [Text]
file3Target =
  [
    "keep me 1",
    "",
    "keep me target 2",
    "keep me target 3",
    "",
    "keep me 4"
  ]

checkContent ::
  Members [Hedgehog IO, Embed IO] r =>
  Path Abs File ->
  [Text] ->
  Sem r ()
checkContent file target =
  (target ===) . lines . toText =<< embed (Text.readFile (toFilePath file))

test_grepReplace :: UnitTest
test_grepReplace =
  proteomeTest do
    dir <- Test.tempDir [reldir|grep/replace|]
    file1 <- Test.tempFile file1Lines [relfile|grep/replace/file1|]
    file2 <- Test.tempFile file2Lines [relfile|grep/replace/file2|]
    file3 <- Test.tempFile file3Lines [relfile|grep/replace/file3|]
    Grep.handleErrors (promptInput replaceEvents (grepWith [] dir pat))
    replaceContent <- currentBufferContent
    assertEq 14 =<< nvimWinGetHeight =<< nvimGetCurrentWin
    9 === length replaceContent
    setCurrentBufferContent $ ([regex|^(delete me.*)$|] . group 0 .~ "") . Text.replace replacePat replacement <$> replaceContent
    proReplaceSave
    proReplaceQuit
    (2 ===) . length =<< filterM buflisted =<< vimGetBuffers
    checkContent file1 file1Target
    checkContent file2 file2Target
    checkContent file3 file3Target

deleteEvents :: [PromptEvent]
deleteEvents =
  Mapping <$> ["k", "<space>", "k", "<space>", "d"]

deleteFile1Lines :: [Text]
deleteFile1Lines =
  [
    "target! 0",
    "",
    "target! 1",
    "",
    "garbage",
    "target! 2",
    "target! 4",
    "garbage"
  ]

deleteFile1Target :: [Text]
deleteFile1Target =
  [
    "target! 0",
    "",
    "garbage",
    "target! 2",
    "garbage"
  ]

test_grepDelete :: UnitTest
test_grepDelete =
  proteomeTest do
    dir <- Test.tempDir [reldir|grep/delete|]
    file1 <- Test.tempFile deleteFile1Lines [relfile|grep/delete/file1|]
    Grep.handleErrors (promptInput deleteEvents (grepWith [] dir pat))
    checkContent file1 deleteFile1Target
