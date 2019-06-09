{-# LANGUAGE QuasiQuotes #-}

module Proteome.PersistBuffers where

import Control.Monad.Catch (MonadThrow)
import Data.Composition ((.:))
import qualified Data.Text as Text (null)
import Path (Abs, Dir, File, Path, Rel, parseRelDir, relfile, toFilePath, (</>))
import Ribosome.Api.Buffer (buflisted, edit)
import Ribosome.Control.Lock (lockOrSkip)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (bufferGetName, vimCommand, vimGetBuffers, vimGetCurrentBuffer)
import Ribosome.Persist (mayPersistLoad, persistStore)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject)
import Proteome.Data.PersistBuffers (PersistBuffers(PersistBuffers))
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import Proteome.Path (existingFile)

projectPaths ::
  MonadThrow m =>
  MonadDeepState s Env m =>
  m (Maybe (Path Abs Dir, Path Rel Dir))
projectPaths =
  examine =<< getL @Env Env.mainProject
  where
    examine (Project (DirProject (ProjectName name) (ProjectRoot root) (Just (ProjectType tpe))) _ _ _) =
      Just . (root,) <$> ((</>) <$> parseRelDir (toString tpe) <*> parseRelDir (toString name))
    examine _ =
      return Nothing

activeBufferFile ::
  NvimE e m =>
  MonadIO m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  m (Maybe (Path Abs File))
activeBufferFile cwd = do
  activeName <- vimGetCurrentBuffer
  existingFile cwd =<< bufferGetName activeName

unsafeStoreBuffers ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Abs Dir ->
  Path Rel Dir ->
  m ()
unsafeStoreBuffers cwd path = do
  active <- activeBufferFile cwd
  names <- traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers
  files <- catMaybes <$> traverse (existingFile cwd) names
  persistStore (path </> [relfile|buffers|]) (PersistBuffers active files)

safeStoreBuffers ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Abs Dir ->
  Path Rel Dir ->
  m ()
safeStoreBuffers =
  lockOrSkip "store-buffers" .: unsafeStoreBuffers

decodePersistBuffers ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m (Maybe PersistBuffers)
decodePersistBuffers path =
  mayPersistLoad (path </> [relfile|buffers|])

restoreBuffers ::
  NvimE e m =>
  PersistBuffers ->
  m ()
restoreBuffers (PersistBuffers current' buffers') = do
  currentBufferName <- bufferGetName =<< vimGetCurrentBuffer
  when (Text.null currentBufferName) $ traverse_ edit (toFilePath <$> current')
  traverse_ add buffers'
  where
    add a =
      vimCommand ("silent! badd " <> toText (toFilePath a))

unsafeLoadBuffers ::
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m ()
unsafeLoadBuffers path = do
  pb <- decodePersistBuffers path
  traverse_ restoreBuffers pb

safeLoadBuffers ::
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  Path Rel Dir ->
  m ()
safeLoadBuffers path =
  lockOrSkip "load-buffers" $ unsafeLoadBuffers path

storeBuffers ::
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadBaseControl IO m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  m ()
storeBuffers =
  traverse_ (uncurry safeStoreBuffers) =<< projectPaths

loadBuffers ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  m ()
loadBuffers =
  traverse_ (safeLoadBuffers . snd) =<< projectPaths
