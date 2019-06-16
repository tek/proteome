{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GrepSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yield)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Api.Window (currentLine)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Grep (proGrepWith)
import Unit (tmuxGuiSpec)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' =
  sleep 0.1 *>
  traverse_ (\ x -> sleep 1 *> yield x) (PromptEvent.Character <$> chars')

chars :: [Text]
chars =
  ["k", "cr"]

promptConfig ::
  MonadIO m =>
  PromptConfig m
promptConfig =
  PromptConfig (promptInput chars) basicTransition noPromptRenderer False

grepSpec :: Proteome ()
grepSpec = do
  dir <- fixture "grep/pro"
  proGrepWith promptConfig (toText dir) "target with spaces"
  l <- currentLine
  gassertEqual 5 l

test_grep :: IO ()
test_grep =
  tmuxGuiSpec grepSpec
