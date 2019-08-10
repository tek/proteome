{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ReplaceSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import qualified Data.Text as Text (replace)
import Ribosome.Api.Buffer (buflisted, currentBufferContent, setCurrentBufferContent)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Nvim.Api.IO (vimGetBuffers)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Grep (proGrepWith)
import Proteome.Grep.Replace (proReplaceQuit, proReplaceSave)
import Unit (tmuxSpec)

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
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

pat :: Text
pat =
  "target"

replacement :: Text
replacement =
  "replaced"

replaceChars :: [Text]
replaceChars =
  ["space", "space", "space", "r"]

file1Lines :: [Text]
file1Lines =
  [
    "garbage 0",
    "target",
    "garbage 1",
    "and target here"
  ]

file2Lines :: [Text]
file2Lines =
  [
    "garbage 2",
    "and target"
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

grepReplaceSpec :: Proteome ()
grepReplaceSpec = do
  dir <- tempDir "grep/replace"
  writeFile (dir <> "/file1") (toString $ unlines file1Lines)
  writeFile (dir <> "/file2") (toString $ unlines file2Lines)
  proGrepWith (promptConfig replaceChars) (toText dir) pat
  replaceContent <- currentBufferContent
  gassertEqual 3 (length replaceContent)
  setCurrentBufferContent $ Text.replace pat replacement <$> replaceContent
  proReplaceSave
  proReplaceQuit
  gassertEqual 2 . length =<< filterM buflisted =<< vimGetBuffers
  checkContent dir file1Target "file1"
  checkContent dir file2Target "file2"
  where
    checkContent dir target name =
      gassertEqual target . lines . toText =<< readFile (dir <> "/" <> name)

test_grepReplace :: IO ()
test_grepReplace =
  tmuxSpec grepReplaceSpec
