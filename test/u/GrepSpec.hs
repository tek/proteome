{-# OPTIONS_GHC -F -pgmF htfpp #-}

module GrepSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Test.Framework
import Ribosome.Api.Window (currentLine)

import Proteome.Data.Env (Proteome)
import Proteome.Grep (proGrepWith)
import Unit (specDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars = do
  lift $ sleep 0.1
  yieldMany (PromptEvent.Character <$> chars)

chars :: [Text]
chars =
  ["cr"]

promptConfig ::
  MonadIO m =>
  PromptConfig m
promptConfig =
  PromptConfig (promptInput chars) basicTransition noPromptRenderer True

grepSpec :: Proteome ()
grepSpec = do
  dir <- fixture "grep/pro"
  proGrepWith promptConfig (toText dir) "target"
  gassertEqual 3 =<< currentLine

test_grep :: IO ()
test_grep =
  specDef grepSpec
