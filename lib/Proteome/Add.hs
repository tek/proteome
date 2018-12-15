module Proteome.Add(
  proAdd,
  proAddCmd,
)
where

import Control.Lens (over)
import Data.List.Utils (split)
import Neovim (nvim_err_writeln', CommandArguments)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (_projects)
import Proteome.Data.Project (
  meta,
  ProjectMetadata(VirtualProject),
  ProjectName(ProjectName),
  ProjectType(ProjectType),
  )
import Proteome.Project.Resolve (resolveProjectFromConfig)
import Proteome.Data.Proteome (Proteome)

add :: ProjectName -> Maybe ProjectType -> Proteome ()
add name tpe = do
  project <- resolveProjectFromConfig Nothing name tpe
  case meta project of
    VirtualProject _ -> return ()
    _ -> Ribo.modify $ over _projects (\p -> p ++ [project])

proAdd :: AddOptions -> Proteome ()
proAdd (AddOptions name tpe) =
  add name (Just tpe)

addFromName :: ProjectName -> Proteome ()
addFromName name = add name Nothing

proAddCmd :: CommandArguments -> String -> Proteome ()
proAddCmd _ spec =
  case split "/" spec of
    [tpe, name] -> add (ProjectName name) (Just (ProjectType tpe))
    [name] -> addFromName (ProjectName name)
    _ -> nvim_err_writeln' $ "invalid project spec: " ++ spec
