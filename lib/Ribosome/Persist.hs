module Ribosome.Persist(
  persistStore,
  persistenceFile,
  persistencePath,
  defaultPersistencePath,
  persistLoad,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), catchE, mapExceptT)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.ByteString.Lazy as B (writeFile, readFile)
import System.FilePath (takeDirectory, (</>))
import System.Directory (getXdgDirectory, XdgDirectory(XdgCache), createDirectoryIfMissing)
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (name)
import Ribosome.Config.Setting (settingE)
import qualified Ribosome.Config.Settings as S (persistenceDir)

liftExceptTIO :: (MonadIO m) => IO a -> ExceptT e m a
liftExceptTIO fa = ExceptT $ fmap Right $ liftIO fa

liftExceptT :: (Functor m) => m a -> ExceptT e m a
liftExceptT fa = ExceptT $ fmap Right $ fa

defaultPersistencePath :: FilePath -> IO FilePath
defaultPersistencePath =
  getXdgDirectory XdgCache

persistencePath :: FilePath -> Ribo e FilePath
persistencePath path = do
  name <- Ribo.name
  let prefixed = name </> path
  custom <- settingE S.persistenceDir
  either (const $ liftIO $ defaultPersistencePath prefixed) (\c -> return $ c </> prefixed) custom

persistenceFile :: FilePath -> Ribo e FilePath
persistenceFile path = do
  file <- persistencePath path
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  return $ file ++ ".json"

persistStore :: ToJSON a => FilePath -> a -> Ribo e ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ B.writeFile file (encode a)

noSuchFile :: Monad m => FilePath -> ExceptT String m a
noSuchFile file = ExceptT $ return $ Left $ "persistence file " ++ file ++ " doesn't exist"

persistLoad :: FromJSON a => FilePath -> (ExceptT String (Ribo e) a)
persistLoad path = do
  file <- liftExceptT $ persistenceFile path
  json <- catchE (liftExceptTIO $ B.readFile file) (const $ noSuchFile file)
  ExceptT $ return $ eitherDecode json
