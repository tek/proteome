module Proteome.Path where

import qualified Data.Text as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  parseAbsDir,
  parseAbsFile,
  parseRelFile,
  toFilePath,
  (</>),
  )
import Path.IO (doesFileExist)

parsePathMaybe ::
  (FilePath -> Either a (Path b t)) ->
  Text ->
  Maybe (Path b t)
parsePathMaybe parser =
  rightToMaybe . parser . toString

parseAbsDirMaybe ::
  Text ->
  Maybe (Path Abs Dir)
parseAbsDirMaybe =
  parsePathMaybe parseAbsDir

absoluteParse ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs File)
absoluteParse cwd spec =
  tryAbsolute <|> tryRelative
  where
    specS =
      toString spec
    tryAbsolute =
      rightToMaybe (parseAbsFile specS)
    tryRelative =
      makeAbsolute <$> rightToMaybe (parseRelFile specS)
    makeAbsolute path =
      cwd </> path

existingFile ::
  MonadIO m =>
  Path Abs Dir ->
  Text ->
  m (Maybe (Path Abs File))
existingFile cwd spec =
  join <$> traverse check (absoluteParse cwd spec)
  where
    check path = do
      exists <- doesFileExist path
      return $ if exists then Just path else Nothing

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

dropSlash :: Path b t -> Text
dropSlash =
  Text.dropWhileEnd ('/' ==) . pathText
