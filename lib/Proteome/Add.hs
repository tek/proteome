module Proteome.Add(
  proAdd,
)
where

import Control.Lens (over)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (_projects)
import Proteome.Data.Project (meta, ProjectMetadata(VirtualProject))
import Proteome.Project.Resolve (resolveProjectFromConfig)
import Proteome.Data.Proteome (Proteome)

proAdd :: AddOptions -> Proteome ()
proAdd (AddOptions name tpe) = do
  project <- resolveProjectFromConfig Nothing name (Just tpe)
  case meta project of
    VirtualProject _ -> return ()
    _ -> Ribo.modify $ over _projects (\p -> p ++ [project])
