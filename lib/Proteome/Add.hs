module Proteome.Add where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Text as Text (splitOn)
import Neovim (CommandArguments(CommandArguments, bang))
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)

import Proteome.Data.AddError (AddError)
import qualified Proteome.Data.AddError as AddError (AddError(InvalidProjectSpec))
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (projects)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(VirtualProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Project.Activate (selectProject)
import Proteome.Project.Resolve (resolveProjectFromConfig)

add ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  ProjectName ->
  Maybe ProjectType ->
  Bool ->
  m ()
add name tpe activate = do
  addDirProject =<< resolveProjectFromConfig Nothing name tpe
  when activate $ selectProject (-1)
  where
    addDirProject (Project (VirtualProject _) _ _ _) =
      return ()
    addDirProject project =
      modifyL @Env Env.projects (\p -> p ++ [project])

proAdd ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  AddOptions ->
  m ()
proAdd (AddOptions name tpe activate) =
  add name (Just tpe) (fromMaybe False activate)

addFromName ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  ProjectName ->
  Bool ->
  m ()
addFromName name =
  add name Nothing

proAddCmd ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e AddError m =>
  CommandArguments ->
  Text ->
  m ()
proAddCmd (CommandArguments bang _ _ _) spec =
  process (Text.splitOn "/" spec)
  where
    process [tpe, name] = do
      dbgs bang
      add (ProjectName name) (Just (ProjectType tpe)) activate
    process [name] =
      addFromName (ProjectName name) activate
    process _ =
      throwHoist (AddError.InvalidProjectSpec spec)
    activate =
      fromMaybe False bang
