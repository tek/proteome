module Config where

import qualified Data.Map as Map (union)
import Ribosome.Test.Embed (TestConfig(tcVariables), Vars, Vars(..), varsFromList, varsUnion)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)
import System.Directory (getCurrentDirectory)

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "proteome"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "proteome"

vars :: IO Vars
vars = do
  base <- getCurrentDirectory
  return $ varsFromList [
    ("proteome_project_base_dirs", toMsgpack [base <> "/test/u/fixtures/projects"]),
    ("proteome_main_project_dir", toMsgpack $ base <> "/test/u/fixtures/projects/haskell/flagellum")
    ]

withVars :: TestConfig -> IO TestConfig
withVars conf = do
  v <- vars
  return conf { tcVariables = varsUnion v (tcVariables conf) }
