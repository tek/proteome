{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GrepSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Normal (normal)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Grep (proGrepWith)
import Unit (tmuxGuiSpec, tmuxSpec)

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
  tmuxGuiSpec grepYankSpec
