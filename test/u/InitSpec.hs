{-# OPTIONS_GHC -F -pgmF htfpp #-}

module InitSpec(htf_thisModulesTests) where

import Config (vars)
import Control.Monad.IO.Class (liftIO)
import Proteome.Data.Project (ProjectName(ProjectName), ProjectType(ProjectType))
import qualified Proteome.Settings as S
import Proteome.Test.Functional (specWith)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Ribo (Ribo)
import Test.Framework

initSpec :: Ribo env ()
initSpec = do
  tpe <- setting S.mainType
  name <- setting S.mainName
  liftIO $ assertEqual name (ProjectName "flagellum")
  liftIO $ assertEqual tpe (ProjectType "haskell")

test_init :: IO ()
test_init =
  vars >>= specWith initSpec
