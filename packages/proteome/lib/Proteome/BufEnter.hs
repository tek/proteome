module Proteome.BufEnter where

import Conc (lock)
import Control.Lens ((.~))
import Data.List.Extra (nub)
import qualified Data.Text as Text (intercalate)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Ribosome (Buffer, Handler, Rpc, RpcError, Settings, resumeHandlerError)
import Ribosome.Api (bufferSetOption, vimGetCurrentBuffer)
import Ribosome.Api.Buffer (bufferIsFile, buflisted)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Settings as Settings

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Project (allProjects)
import Proteome.Settings (tagsFileName)

data MruLock =
  MruLock
  deriving stock (Eq, Show)

setBufferTags ::
  Member Rpc r =>
  [Path Abs File] ->
  Sem r ()
setBufferTags tags = do
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "tags" (Text.intercalate "," (toText . toFilePath <$> tags))

projectRoot :: Project -> Maybe (Path Abs Dir)
projectRoot (Project (DirProject _ (ProjectRoot root) _) _ _ _) = Just root
projectRoot _ = Nothing

updateBufferMru ::
  Members [AtomicState Env, Sync MruLock, Rpc !! RpcError, Resource] r =>
  Buffer ->
  Sem r ()
updateBufferMru buffer = do
  lock MruLock do
    old <- atomicGets Env.buffers
    new <- filterM buflisted (nub (buffer : old))
    atomicModify' (#buffers .~ new)

updateBuffers ::
  Members [AtomicState Env, Sync MruLock, Rpc, Rpc !! RpcError, Resource] r =>
  Sem r ()
updateBuffers = do
  current <- vimGetCurrentBuffer
  whenM (bufferIsFile current) (updateBufferMru current)

bufEnter ::
  Members [AtomicState Env, Sync MruLock, Rpc, Rpc !! RpcError, Settings !! SettingError, Resource] r =>
  Handler r ()
bufEnter = do
  updateBuffers
  roots <- mapMaybe projectRoot <$> allProjects
  name <- resumeHandlerError (Settings.get tagsFileName)
  setBufferTags ((</> name) <$> roots)
