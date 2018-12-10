{-# LANGUAGE DeriveGeneric #-}

module Proteome.PersistBuffers(
  loadBuffers,
  storeBuffers,
  PersistBuffers(PersistBuffers),
) where

import GHC.Generics
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toEncoding), FromJSON, genericToEncoding, defaultOptions)
import Data.Foldable (traverse_)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Neovim (vim_get_current_buffer', vim_get_buffers', buffer_get_name', vim_command')
import Ribosome.Api.Buffer (edit)
import Ribosome.Persist (persistStore, persistLoad)
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

storeBuffers' :: FilePath -> Proteome ()
storeBuffers' path = do
  active <- vim_get_current_buffer'
  activeName <- buffer_get_name' active
  activeIsFile <- liftIO $ doesFileExist activeName
  let current' = if activeIsFile then Just activeName else Nothing
  all' <- vim_get_buffers' >>= traverse buffer_get_name'
  files <- liftIO $ filterM doesFileExist all'
  persistStore (path </> "buffers") (PersistBuffers current' files)

decodePersistBuffers :: FilePath -> Proteome (Either String PersistBuffers)
decodePersistBuffers path = persistLoad (path </> "buffers")

restoreBuffers :: PersistBuffers -> Proteome ()
restoreBuffers (PersistBuffers current' buffers') = do
  traverse_ (\a -> vim_command' ("badd " ++ a)) buffers'
  mapM_ edit current'

loadBuffers' :: FilePath -> Proteome ()
loadBuffers' path = do
  pb <- decodePersistBuffers path
  mapM_ restoreBuffers pb

storeBuffers :: Proteome ()
storeBuffers = do
  sub <- projectSubPath
  mapM_ storeBuffers' sub

loadBuffers :: Proteome ()
loadBuffers = do
  sub <- projectSubPath
  mapM_ loadBuffers' sub
