module Proteome.PersistBuffers where

import Conc (Lock, lockOrSkip_)
import qualified Data.Text as Text (null)
import Exon (exon)
import qualified Log
import Path (Abs, Dir, File, Path, Rel, parseRelDir, relfile, toFilePath, (</>))
import Ribosome (Rpc, RpcError)
import Ribosome.Api (bufferGetName, vimCommand, vimGetCurrentBuffer)
import Ribosome.Api.Buffer (bufferForFile, buflisted, edit)
import qualified Ribosome.Data.FileBuffer as FileBuffer
import Ribosome.Effect.Persist (Persist)
import qualified Ribosome.Persist as Persist

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (buffers, mainProject)
import Proteome.Data.PersistBuffers (PersistBuffers (PersistBuffers))
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import Proteome.Path (existingFile)

data StoreBuffersLock =
  StoreBuffersLock
  deriving stock (Eq, Show)

data LoadBuffersLock =
  LoadBuffersLock
  deriving stock (Eq, Show)

file :: Path Rel File
file =
  [relfile|buffers.json|]

projectPaths ::
  Member (AtomicState Env) r =>
  Sem r (Maybe (Path Abs Dir, Path Rel Dir))
projectPaths =
  examine <$> atomicGets (.mainProject)
  where
    examine (Project (DirProject (ProjectName name) (ProjectRoot root) (Just (ProjectType tpe))) _ _ _) =
      (root,) <$> ((</>) <$> parseRelDir (toString tpe) <*> parseRelDir (toString name))
    examine _ =
      Nothing

storeBuffers ::
  Member (Persist PersistBuffers) r =>
  Members [Lock @@ StoreBuffersLock, AtomicState Env, Rpc, Rpc !! RpcError, Resource, Embed IO] r =>
  Sem r ()
storeBuffers =
  tag $ lockOrSkip_ $ projectPaths >>= traverse_ \ (cwd, path) -> do
    names <- traverse bufferGetName =<< filterM buflisted =<< atomicGets (.buffers)
    files <- catMaybes <$> traverse (existingFile cwd) names
    Persist.store (Just (path </> file)) (PersistBuffers (listToMaybe files) files)

decodePersistBuffers ::
  Member (Persist PersistBuffers) r =>
  Path Rel Dir ->
  Sem r (Maybe PersistBuffers)
decodePersistBuffers path =
  Persist.load (Just (path </> file))

restoreBuffers ::
  Members [Rpc, AtomicState Env, Log] r =>
  PersistBuffers ->
  Sem r ()
restoreBuffers (PersistBuffers active rest) = do
  Log.debug [exon|Restoring buffers. Active: #{show active}|]
  traverse_ loadActive active
  traverse_ add rest
  buffers <- traverse bufferForFile rest
  atomicModify' (#buffers .~ ((.buffer) <$> catMaybes buffers))
  where
    add a =
      vimCommand ("silent! badd " <> toText (toFilePath a))
    loadActive path = do
      currentBufferName <- bufferGetName =<< vimGetCurrentBuffer
      when (Text.null currentBufferName) (edit path)

loadBuffers ::
  Members [Persist PersistBuffers, Lock @@ LoadBuffersLock, Rpc, AtomicState Env, Log, Resource] r =>
  Sem r ()
loadBuffers =
  tag $ lockOrSkip_ $ projectPaths >>= traverse_ \ (_, path) ->
    traverse_ restoreBuffers =<< decodePersistBuffers path
