module Ribosome.Persist(
  persistStore,
  persistenceFile,
  persistencePath,
  defaultPersistencePath,
  persistLoad,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Lazy as B (writeFile)
import System.FilePath (takeDirectory, (</>))
import System.Directory (getXdgDirectory, XdgDirectory(XdgCache), createDirectoryIfMissing)
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribo as Ribo (name)
import Ribosome.Config.Setting (settingE)
import qualified Ribosome.Config.Settings as S (persistenceDir)

defaultPersistencePath :: FilePath -> IO FilePath
defaultPersistencePath =
  getXdgDirectory XdgCache

persistencePath :: FilePath -> Ribo e FilePath
persistencePath path = do
  name <- Ribo.name
  let prefixed = name </> path
  custom <- settingE S.persistenceDir
  either (\_ -> liftIO $ defaultPersistencePath prefixed) (\c -> return $ c </> prefixed) custom

persistenceFile :: FilePath -> Ribo e FilePath
persistenceFile path = do
  file <- persistencePath path
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  return $ file ++ ".json"

persistStore :: ToJSON a => FilePath -> a -> Ribo e ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ B.writeFile file (encode a)

persistLoad :: FromJSON a => FilePath -> Ribo e a
persistLoad _ = undefined
