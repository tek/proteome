{-# LANGUAGE RecordWildCards #-}

module Proteome.Add(
  proAdd,
  proAddCmd,
)
where

import Control.Monad (when)
import Control.Lens (over)
import Data.List.Utils (split)
import Neovim (nvim_err_writeln', CommandArguments(CommandArguments, bang))
import qualified Ribosome.Control.Ribo as Ribo (modify)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (_projects)
import Proteome.Data.Project (
  meta,
  ProjectMetadata(VirtualProject),
  ProjectName(ProjectName),
  ProjectType(ProjectType),
  )
import Proteome.Project.Activate (selectProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)
import Proteome.Data.Proteome (Proteome)

add :: ProjectName -> Maybe ProjectType -> Bool -> Proteome ()
add name tpe activate = do
  project <- resolveProjectFromConfig Nothing name tpe
  case meta project of
    VirtualProject _ -> return ()
    _ -> Ribo.modify $ over _projects (\p -> p ++ [project])
  when activate $ selectProject (-1)

proAdd :: AddOptions -> Proteome ()
proAdd (AddOptions name tpe activate) =
  add name (Just tpe) activate

addFromName :: ProjectName -> Bool -> Proteome ()
addFromName name = add name Nothing

proAddCmd :: CommandArguments -> String -> Proteome ()
proAddCmd args spec =
  case split "/" spec of
    [tpe, name] -> add (ProjectName name) (Just (ProjectType tpe)) activate
    [name] -> addFromName (ProjectName name) activate
    _ -> nvim_err_writeln' $ "invalid project spec: " ++ spec
  where
    activate = case args of
      CommandArguments { bang = Just True, .. } -> True
      _ -> False
