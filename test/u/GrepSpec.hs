{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GrepSpec (htf_thisModulesTests) where

import Conduit (ConduitT, runConduit, sinkList, yield, yieldMany, (.|))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Data.GrepOutputLine (GrepOutputLine(GrepOutputLine))
import Proteome.Grep (proGrepWith)
import Proteome.Grep.Line (uniqueGrepLines)
import Proteome.Grep.Process (grepMenuItems)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Unit (specDef, tmuxSpec)

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
  "target with spaces"

jumpChars :: [Text]
jumpChars =
  ["k", "cr"]

grepJumpSpec :: Proteome ()
grepJumpSpec = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig jumpChars) (toText dir) pat
  l <- currentLine
  gassertEqual 5 l

test_grepJump :: IO ()
test_grepJump =
  tmuxSpec grepJumpSpec

yankChars :: [Text]
yankChars =
  ["k", "y"]

grepYankSpec :: Proteome ()
grepYankSpec = do
  dir <- fixture "grep/pro"
  proGrepWith (promptConfig yankChars) (toText dir) pat
  normal "P"
  l <- currentBufferContent
  gassertEqual ["line 6 " <> pat, ""] l

test_grepYank :: IO ()
test_grepYank =
  tmuxSpec grepYankSpec

grepDuplicatesSpec :: Proteome ()
grepDuplicatesSpec = do
  dir <- toText <$> fixture "grep/duplicates"
  outputDupes <- runConduit $ proc dir .| sinkList
  gassertEqual 2 (length (join outputDupes))
  outputUnique <- runConduit $ proc dir .| uniqueGrepLines .| sinkList
  gassertEqual 1 (length (join outputUnique))
  output3 <- runConduit $ yield (item 1) *> yield (item 2) .| uniqueGrepLines .| sinkList
  gassertEqual 1 (length (join output3))
  where
    proc dir =
      grepMenuItems dir "rg" ["--vimgrep", "--no-heading", "target", dir]
    item col =
      [MenuItem (GrepOutputLine "/path/to/file" 0 (Just col) "target") "" ""]

test_grepDuplicates :: IO ()
test_grepDuplicates =
  specDef grepDuplicatesSpec
