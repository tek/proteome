module Proteome.Test.Unit(
  spec,
  specWith,
) where

import UnliftIO.STM (newTVarIO)
import Ribosome.Test.Embed (Vars)
import Ribosome.Test.Unit (unitSpec)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Env (Env)
import Proteome.Test.Config (defaultTestConfig, defaultTestConfigWith)

spec :: Env -> Proteome () -> IO ()
spec e s = do
  t <- newTVarIO e
  unitSpec defaultTestConfig t s

specWith :: Env -> Proteome () -> Vars -> IO ()
specWith e s vars = do
  t <- newTVarIO e
  unitSpec (defaultTestConfigWith vars) t s
