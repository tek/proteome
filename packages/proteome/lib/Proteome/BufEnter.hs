module Proteome.BufEnter where

import Conc (Lock, lock)
import Data.List.Extra (nub)
import qualified Data.Text as Text (intercalate)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Ribosome (Buffer, Handler, Rpc, RpcError, Settings, resumeReport)
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

data Mru =
  Mru
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
  Members [AtomicState Env, Lock @@ Mru, Rpc !! RpcError, Resource] r =>
  Buffer ->
  Sem r ()
updateBufferMru buffer = do
  tag $ lock do
    old <- atomicGets (.buffers)
    new <- filterM buflisted (nub (buffer : old))
    atomicModify' (#buffers .~ new)

updateBuffers ::
  Members [AtomicState Env, Lock @@ Mru, Rpc, Rpc !! RpcError, Resource] r =>
  Sem r ()
updateBuffers = do
  current <- vimGetCurrentBuffer
  whenM (bufferIsFile current) (updateBufferMru current)

bufEnter ::
  Members [AtomicState Env, Lock @@ Mru, Rpc !! RpcError, Settings !! SettingError, Resource] r =>
  Handler r ()
bufEnter =
  resumeReport @Rpc do
    updateBuffers
    roots <- mapMaybe projectRoot <$> allProjects
    name <- resumeReport (Settings.get tagsFileName)
    setBufferTags ((</> name) <$> roots)
