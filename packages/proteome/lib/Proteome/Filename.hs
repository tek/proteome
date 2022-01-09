module Proteome.Filename where

import qualified Chronos
import Control.Monad.Catch (MonadThrow)
import Data.MessagePack (Object)
import qualified Data.Text as Text
import Neovim (CommandArguments (CommandArguments))
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  addExtension,
  dirname,
  filename,
  parent,
  parseAbsDir,
  parseRelDir,
  reldir,
  relfile,
  splitExtension,
  toFilePath,
  (</>),
  )
import Path.IO (copyFile, doesDirExist, doesFileExist, ensureDir, removeFile)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (bufferGetNumber, bufferSetName, vimCallFunction, vimCommand, vimGetCurrentBuffer)
import Ribosome.Persist (persistencePath)

import Proteome.Data.Env (Proteome)
import qualified Proteome.Data.FilenameError as FilenameError
import Proteome.Data.FilenameError (FilenameError)
import Proteome.Path (
  absoluteParse,
  absoluteParseDir,
  parseAbsDirMaybe,
  parseAbsFileMaybe,
  parseRelDirMaybe,
  parseRelFileMaybe,
  pathText,
  )

data BufPath =
  BufPath (Path Abs File) [Text]
  deriving stock (Eq, Show)

bufDir :: BufPath -> Path Abs Dir
bufDir (BufPath f _) =
  parent f

data NameSpec =
  Star (Path Rel File)
  |
  Literal (Path Rel File)
  deriving stock (Eq, Show)

rawNameSpec ::
  NameSpec ->
  Path Rel File
rawNameSpec = \case
  Star f -> f
  Literal f -> f

data Modification =
  Filename (Path Rel File) (Path Rel Dir) NameSpec [Text]
  |
  Dir (Path Abs Dir)
  |
  File (Path Abs File)
  |
  Container Int (Path Rel Dir)
  deriving stock (Eq, Show)

nameSpec :: Path Rel File -> NameSpec
nameSpec p =
  if p == [relfile|*|] then Star p else Literal p

dotsInPath :: Text -> Int
dotsInPath path =
  Text.length (Text.filter (== '.') (Text.drop 1 path))

absoluteDir :: Text -> Maybe Modification
absoluteDir =
  fmap Dir . parseAbsDirMaybe

absoluteFile :: Text -> Maybe Modification
absoluteFile =
  fmap File . parseAbsFileMaybe

relativeDir :: Path Abs Dir -> Text -> Maybe Modification
relativeDir cwd spec = do
  rel <- parseRelDirMaybe spec
  pure (Dir (cwd </> rel))

relativeFile :: Path Abs Dir -> Text -> Maybe Modification
relativeFile cwd spec = do
  rel <- parseRelFileMaybe spec
  pure (File (cwd </> rel))

splitExtensions :: Path b File -> (Path b File, [Text])
splitExtensions =
  spin []
  where
    spin exts f =
      case splitExtension f of
        Just (f', e) -> spin (toText e : exts) f'
        Nothing -> (f, exts)

addExtensions ::
  Path b File ->
  [Text] ->
  Maybe (Path b File)
addExtensions name exts =
  foldlM (flip addExtension) name (toString <$> exts)

nameOnly :: Text -> Maybe Modification
nameOnly spec = do
  rel <- parseRelFileMaybe spec
  let (name, exts) = splitExtensions (filename rel)
  Just (Filename rel (parent rel) (nameSpec name) exts)

maybeDir ::
  MonadIO m =>
  Path Abs Dir ->
  Text ->
  m Bool
maybeDir cwd spec =
  fromMaybe False <$> traverse doesDirExist (absoluteParseDir cwd spec)

regularModification ::
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs Dir ->
  Text ->
  m Modification
regularModification cwd spec = do
  existingDir <- maybeDir cwd spec
  hoistMaybe (FilenameError.InvalidPathSpec spec) (cons existingDir spec)
  where
    cons existingDir =
      if name then nameOnly else
      if absolute
      then if explicitDir || existingDir then absoluteDir else absoluteFile
      else if explicitDir then (relativeDir cwd) else (relativeFile cwd)
    name =
      not (Text.any ('/' ==) spec)
    absolute =
      Text.take 1 spec == "/"
    explicitDir =
      Text.takeEnd 1 spec == "/"

directorySelector :: Text -> (Int, Text)
directorySelector =
  first Text.length . Text.span ('^' ==)

modification ::
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Path Abs Dir ->
  Text ->
  m Modification
modification raw cwd (Text.strip -> spec) =
  case directorySelector spec of
    (n, _) | n == 0 || raw ->
      regularModification cwd spec
    (n, name) -> do
      dir <- hoistMaybe (FilenameError.InvalidPathSpec name) (parseRelDir (toString name))
      pure (Container n dir)

checkBufferPath ::
  NvimE e m =>
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs Dir ->
  m (Path Abs File)
checkBufferPath cwd = do
  name <- currentBufferName
  path <- hoistMaybe FilenameError.BufferPathInvalid (absoluteParse cwd name)
  ifM (doesFileExist path) (pure path) (throwHoist FilenameError.BufferPathInvalid)

withExtension ::
  BufPath ->
  [Text] ->
  NameSpec ->
  Maybe (Path Rel File)
withExtension (BufPath bufName bufExts) exts = \case
  Star _ ->
    addExtensions (filename bufName) (take (length bufExts - length exts) bufExts ++ exts)
  Literal name ->
    addExtensions name (exts ++ drop (length exts) bufExts)

renameInplace ::
  MonadDeepError e FilenameError m =>
  Bool ->
  Path Rel File ->
  BufPath ->
  Path Rel Dir ->
  NameSpec ->
  [Text] ->
  m (Path Abs File)
renameInplace raw spec bufPath destDir newName exts = do
  rel <-
    if raw
    then pure (destDir </> spec)
    else hoistMaybe FilenameError.BufferPathInvalid (withExtension bufPath exts newName)
  pure (bufDir bufPath </> rel)

replaceDir ::
  MonadDeepError e FilenameError m =>
  Int ->
  Path Rel Dir ->
  Path Abs File ->
  m (Path Abs File)
replaceDir index name file = do
  dir <- spin (parent file) index
  pure (dir </> filename file)
  where
    spin d _ | parent d == d =
      throwHoist (FilenameError.InvalidPathSpec "not enough directory segments in buffer path")
    spin d i | i <= 1 =
      pure (parent d </> name)
    spin d i = do
      sub <- spin (parent d) (i - 1)
      pure (sub </> dirname d)

assemblePath ::
  MonadDeepError e FilenameError m =>
  Bool ->
  Path Abs File ->
  Modification ->
  m (Path Abs File)
assemblePath raw bufPath = \case
  Filename rawSpec destDir newName exts ->
    renameInplace raw rawSpec (uncurry BufPath (splitExtensions bufPath)) destDir newName exts
  Dir dir ->
    pure (dir </> filename bufPath)
  File file ->
    pure file
  Container index name ->
    replaceDir index name bufPath

ensureDestinationEmpty ::
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  m ()
ensureDestinationEmpty path =
  whenM (doesFileExist path) (throwHoist (FilenameError.Exists (pathText path)))

prepareDestination ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Path Abs File ->
  m ()
prepareDestination path =
  ifM exists (ensureDestinationEmpty path) create
  where
    exists =
      doesDirExist dir
    create =
      tryHoistAnyAs (FilenameError.CreateDir (pathText dir)) (ensureDir dir)
    dir =
      parent path

getCwd ::
  NvimE e m =>
  MonadDeepError e FilenameError m =>
  m (Path Abs Dir)
getCwd =
  hoistEitherAs FilenameError.BadCwd =<< parseAbsDir <$> nvimCwd

smartModification ::
  NvimE e m =>
  MonadIO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Text ->
  m Modification
smartModification raw spec = do
  cwd <- getCwd
  modification raw cwd spec

trashModification ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e FilenameError m =>
  m Modification
trashModification = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  let original = pathText (filename bufPath)
  Chronos.Time stamp <- liftIO Chronos.now
  trashFile <- hoistMaybe FilenameError.BufferPathInvalid (parseRelFileMaybe [text|#{stamp}_#{original}|])
  trashPath <- persistencePath ([reldir|trash|] </> trashFile)
  let trashDir = parent trashPath
  tryHoistAnyAs (FilenameError.CreateDir (pathText trashDir)) (ensureDir trashDir)
  pure (File trashPath)

pathsForMod ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Modification ->
  m (Path Abs File, Path Abs File)
pathsForMod raw mod' = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  path <- assemblePath raw bufPath mod'
  prepareDestination path
  pure (bufPath, path)

updateBuffer ::
  NvimE e m =>
  Path Abs File ->
  m ()
updateBuffer path = do
  buf <- vimGetCurrentBuffer
  bufferSetName buf (pathText path)
  vimCommand "silent write!"

relocate ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Text ->
  Modification ->
  (Path Abs File -> Path Abs File -> m ()) ->
  m ()
relocate raw action mod' run = do
  (bufPath, destPath) <- pathsForMod raw mod'
  tryHoistAny (FilenameError.ActionFailed action . show) (run bufPath destPath)

moveFile ::
  MonadIO m =>
  Path Abs File ->
  Path Abs File ->
  m ()
moveFile src dest = do
  copyFile src dest
  removeFile src

move ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Modification ->
  m ()
move raw mod' = do
  relocate raw "move" mod' \ buf dest -> do
    vimCommand "silent write!"
    moveFile buf dest
    updateBuffer dest

copy ::
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e FilenameError m =>
  Bool ->
  Modification ->
  m ()
copy raw mod' =
  relocate raw "copy" mod' \ src dest -> do
    copyFile src dest
    view :: Object <- vimCallFunction "winsaveview" []
    edit (toFilePath dest)
    vimCallFunction "winrestview" [view]

proMove ::
  CommandArguments ->
  Text ->
  Proteome ()
proMove (CommandArguments bang _ _ _) =
  move raw <=< smartModification raw
  where
    raw =
      fromMaybe False bang

proCopy ::
  CommandArguments ->
  Text ->
  Proteome ()
proCopy (CommandArguments bang _ _ _) =
  copy raw <=< smartModification raw
  where
    raw =
      fromMaybe False bang

proRemove ::
  Proteome ()
proRemove = do
  move False =<< trashModification
  buf <- vimGetCurrentBuffer
  ignoreError @RpcError do
    number <- bufferGetNumber buf
    vimCommand ("silent! noautocmd bwipeout! " <> show number)
