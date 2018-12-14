{-# LANGUAGE DeriveGeneric #-}

module Proteome.PersistBuffers(
  loadBuffers,
  storeBuffers,
  PersistBuffers(PersistBuffers),
) where

import GHC.Generics
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (ToJSON(toEncoding), FromJSON, genericToEncoding, defaultOptions)
import Data.Foldable (traverse_)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Neovim (vim_get_current_buffer', vim_get_buffers', buffer_get_name', vim_command')
import Ribosome.Api.Buffer (edit, buflisted)
import Ribosome.Persist (persistStore, persistLoad)
import Ribosome.Data.Ribo (lockOrSkip)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project(Project),
  ProjectMetadata(DirProject),
  ProjectName(ProjectName),
  ProjectType(ProjectType)
  )
import Proteome.Env (getMainProject)

data PersistBuffers =
  PersistBuffers {
    current :: Maybe FilePath,
    buffers :: [FilePath]
  }
  deriving (Eq, Generic, Show)

instance ToJSON PersistBuffers where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PersistBuffers

projectSubPath :: Proteome (Maybe FilePath)
projectSubPath = do
  main <- getMainProject
  return $ case main of
    Project (DirProject (ProjectName name) _ (Just (ProjectType tpe))) _ _ _ -> Just $ tpe </> name
    _ -> Nothing

unsafeStoreBuffers :: FilePath -> Proteome ()
unsafeStoreBuffers path = do
  active <- vim_get_current_buffer'
  activeName <- buffer_get_name' active
  activeIsFile <- liftIO $ doesFileExist activeName
  let current' = if activeIsFile then Just activeName else Nothing
  all' <- vim_get_buffers' >>= filterM buflisted >>= traverse buffer_get_name'
  files <- liftIO $ filterM doesFileExist all'
  persistStore (path </> "buffers") (PersistBuffers current' files)

safeStoreBuffers :: FilePath -> Proteome ()
safeStoreBuffers path =
  lockOrSkip "store-buffers" $ unsafeStoreBuffers path

decodePersistBuffers :: FilePath -> Proteome (Either String PersistBuffers)
decodePersistBuffers path = runExceptT $ persistLoad (path </> "buffers")

restoreBuffers :: PersistBuffers -> Proteome ()
restoreBuffers (PersistBuffers current' buffers') = do
  active <- vim_get_current_buffer'
  name <- buffer_get_name' active
  when (null name) $ mapM_ edit current'
  traverse_ (\a -> vim_command' ("silent! badd " ++ a)) buffers'

unsafeLoadBuffers :: FilePath -> Proteome ()
unsafeLoadBuffers path = do
  pb <- decodePersistBuffers path
  mapM_ restoreBuffers pb

safeLoadBuffers :: FilePath -> Proteome ()
safeLoadBuffers path =
  lockOrSkip "load-buffers" $ unsafeLoadBuffers path

storeBuffers :: Proteome ()
storeBuffers = do
  sub <- projectSubPath
  mapM_ safeStoreBuffers sub

loadBuffers :: Proteome ()
loadBuffers = do
  sub <- projectSubPath
  mapM_ safeLoadBuffers sub
