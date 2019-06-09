module Proteome.Path where

import Control.Monad.Catch (MonadThrow)
import Path (
  Abs,
  Dir,
  File,
  Path,
  parseAbsDir,
  parseAbsFile,
  parseRelFile,
  (</>),
  )
import Path.IO (doesFileExist)
import Ribosome.Control.Exception (tryAny)

parsePathMaybe ::
  MonadThrow m =>
  MonadBaseControl IO m =>
  (FilePath -> m (Path b t)) ->
  Text ->
  m (Maybe (Path b t))
parsePathMaybe parser =
  rightToMaybe <$$> tryAny . parser . toString

parseAbsDirMaybe ::
  MonadThrow m =>
  MonadBaseControl IO m =>
  Text ->
  m (Maybe (Path Abs Dir))
parseAbsDirMaybe =
  parsePathMaybe parseAbsDir

absoluteParse ::
  MonadThrow m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  Text ->
  m (Maybe (Path Abs File))
absoluteParse cwd spec =
  either (const tryRelative) pure =<< tryAbsolute
  where
    specS =
      toString spec
    tryAbsolute =
      tryAny $ Just <$> parseAbsFile specS
    tryRelative =
      rightToMaybe <$> tryAny (makeAbsolute <$> parseRelFile specS)
    makeAbsolute path =
      cwd </> path

existingFile ::
  MonadIO m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  Path Abs Dir ->
  Text ->
  m (Maybe (Path Abs File))
existingFile cwd spec =
  join <$> (traverse check =<< absoluteParse cwd spec)
  where
    check path = do
      exists <- doesFileExist path
      return $ if exists then Just path else Nothing
