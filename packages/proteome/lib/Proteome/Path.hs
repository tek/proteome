module Proteome.Path where

import qualified Data.Text as Text
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  parent,
  parseAbsDir,
  parseAbsFile,
  parseRelDir,
  parseRelFile,
  (</>),
  )
import Path.IO (doesFileExist)
import Ribosome (pathText)

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

parseAbsFileMaybe ::
  Text ->
  Maybe (Path Abs File)
parseAbsFileMaybe =
  parsePathMaybe parseAbsFile

parseRelDirMaybe ::
  Text ->
  Maybe (Path Rel Dir)
parseRelDirMaybe =
  parsePathMaybe parseRelDir

parseRelFileMaybe ::
  Text ->
  Maybe (Path Rel File)
parseRelFileMaybe =
  parsePathMaybe parseRelFile

absoluteParseDir ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs Dir)
absoluteParseDir cwd spec =
  tryAbsolute <|> tryRelative
  where
    specS =
      toString spec
    tryAbsolute =
      rightToMaybe (parseAbsDir specS)
    tryRelative =
      makeAbsolute <$> rightToMaybe (parseRelDir specS)
    makeAbsolute path =
      cwd </> path

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
      pure $ if exists then Just path else Nothing

dropSlash :: Path b t -> Text
dropSlash =
  Text.dropWhileEnd ('/' ==) . pathText

rootPathSegment :: Path b Dir -> Path b Dir
rootPathSegment p =
  if parent (parent p) == parent p
  then p
  else rootPathSegment (parent p)
