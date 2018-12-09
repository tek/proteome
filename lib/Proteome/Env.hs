module Proteome.Env(
  getMainProject,
) where

import qualified Ribosome.Data.Ribo as Ribo (inspect)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.Project(Project)
import Proteome.Data.Proteome (Proteome)

getMainProject :: Proteome Project
getMainProject =
  Ribo.inspect Env.mainProject
