module Proteome.Filename where

import qualified Chronos
import qualified Data.Text as Text
import Exon (exon)
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
  parseRelDir,
  reldir,
  relfile,
  splitExtension,
  (</>),
  )
import Path.IO (copyFile, doesDirExist, doesFileExist, ensureDir, removeFile)
import Ribosome (
  Bang (Bang),
  Handler,
  HostError,
  Rpc,
  RpcError,
  mapHandlerError,
  pathText,
  reportError,
  resumeHandlerError,
  rpcErrorMessage,
  )
import Ribosome.Api (bufferSetName, vimCallFunction, vimCommand, vimGetCurrentBuffer, wipeBuffer)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Host.Modify (silent)
import Ribosome.Persist (PersistPath, persistPath)

import qualified Proteome.Data.FilenameError as FilenameError
import Proteome.Data.FilenameError (FilenameError)
import Proteome.Path (
  absoluteParse,
  absoluteParseDir,
  parseAbsDirMaybe,
  parseAbsFileMaybe,
  parseRelDirMaybe,
  parseRelFileMaybe,
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
  Member (Embed IO) r =>
  Path Abs Dir ->
  Text ->
  Sem r Bool
maybeDir cwd spec =
  fromMaybe False <$> traverse doesDirExist (absoluteParseDir cwd spec)

regularModification ::
  Members [Stop FilenameError, Embed IO] r =>
  Path Abs Dir ->
  Text ->
  Sem r Modification
regularModification cwd spec = do
  existingDir <- maybeDir cwd spec
  stopNote (FilenameError.InvalidPathSpec spec) (cons existingDir spec)
  where
    cons existingDir
      | name = nameOnly
      | absolute = if explicitDir || existingDir then absoluteDir else absoluteFile
      | explicitDir = relativeDir cwd
      | otherwise = relativeFile cwd
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
  Members [Stop FilenameError, Embed IO] r =>
  Bool ->
  Path Abs Dir ->
  Text ->
  Sem r Modification
modification raw cwd (Text.strip -> spec) =
  case directorySelector spec of
    (n, _) | n == 0 || raw ->
      regularModification cwd spec
    (n, name) -> do
      dir <- stopNote (FilenameError.InvalidPathSpec name) (parseRelDir (toString name))
      pure (Container n dir)

checkBufferPath ::
  Members [Rpc, Stop FilenameError, Embed IO] r =>
  Path Abs Dir ->
  Sem r (Path Abs File)
checkBufferPath cwd = do
  name <- currentBufferName
  path <- stopNote FilenameError.BufferPathInvalid (absoluteParse cwd name)
  ifM (doesFileExist path) (pure path) (stop FilenameError.BufferPathInvalid)

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
  Member (Stop FilenameError) r =>
  Bool ->
  Path Rel File ->
  BufPath ->
  Path Rel Dir ->
  NameSpec ->
  [Text] ->
  Sem r (Path Abs File)
renameInplace raw spec bufPath destDir newName exts = do
  rel <-
    if raw
    then pure (destDir </> spec)
    else stopNote FilenameError.BufferPathInvalid (withExtension bufPath exts newName)
  pure (bufDir bufPath </> rel)

replaceDir ::
  Member (Stop FilenameError) r =>
  Int ->
  Path Rel Dir ->
  Path Abs File ->
  Sem r (Path Abs File)
replaceDir index name file = do
  dir <- spin (parent file) index
  pure (dir </> filename file)
  where
    spin d _ | parent d == d =
      stop (FilenameError.InvalidPathSpec "not enough directory segments in buffer path")
    spin d i | i <= 1 =
      pure (parent d </> name)
    spin d i = do
      sub <- spin (parent d) (i - 1)
      pure (sub </> dirname d)

assemblePath ::
  Member (Stop FilenameError) r =>
  Bool ->
  Path Abs File ->
  Modification ->
  Sem r (Path Abs File)
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
  Members [Stop FilenameError, Embed IO] r =>
  Path Abs File ->
  Sem r ()
ensureDestinationEmpty path =
  whenM (doesFileExist path) (stop (FilenameError.Exists (pathText path)))

prepareDestination ::
  Members [Stop FilenameError, Embed IO] r =>
  Path Abs File ->
  Sem r ()
prepareDestination path =
  ifM exists (ensureDestinationEmpty path) create
  where
    exists =
      doesDirExist dir
    create =
      stopTryAny (const (FilenameError.CreateDir (pathText dir))) (ensureDir dir)
    dir =
      parent path

getCwd ::
  Members [Stop FilenameError, Rpc !! RpcError] r =>
  Sem r (Path Abs Dir)
getCwd =
  resumeHoistAs FilenameError.BadCwd nvimCwd

smartModification ::
  Members [Stop FilenameError, Rpc !! RpcError, Embed IO] r =>
  Bool ->
  Text ->
  Sem r Modification
smartModification raw spec = do
  cwd <- getCwd
  modification raw cwd spec

trashModification ::
  Members [Stop FilenameError, Rpc, Rpc !! RpcError, PersistPath, Embed IO] r =>
  Sem r Modification
trashModification = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  let original = pathText (filename bufPath)
  Chronos.Time stamp <- liftIO Chronos.now
  trashFile <- stopNote FilenameError.BufferPathInvalid (parseRelFileMaybe [exon|#{show stamp}_#{original}|])
  trashDir <- persistPath (Just [reldir|trash|])
  let trashPath = trashDir </> trashFile
  stopTryAny (const (FilenameError.CreateDir (pathText trashDir))) (ensureDir trashDir)
  pure (File trashPath)

pathsForMod ::
  Members [Stop FilenameError, Rpc, Rpc !! RpcError, Embed IO] r =>
  Bool ->
  Modification ->
  Sem r (Path Abs File, Path Abs File)
pathsForMod raw modi = do
  cwd <- getCwd
  bufPath <- checkBufferPath cwd
  path <- assemblePath raw bufPath modi
  prepareDestination path
  pure (bufPath, path)

writeBuffer ::
  Members [Stop FilenameError, Rpc !! RpcError] r =>
  Text ->
  Sem r ()
writeBuffer action =
  err "Couldn't write buffer" $ silent do
    vimCommand "write!"
  where
    err msg =
      resumeHoist \ e -> FilenameError.ActionFailed action [exon|#{msg}: #{rpcErrorMessage e}|]

updateBuffer ::
  Member Rpc r =>
  Path Abs File ->
  Sem r ()
updateBuffer path = do
  buf <- vimGetCurrentBuffer
  bufferSetName buf (pathText path)
  silent do
    vimCommand "write!"

relocate ::
  Members [Stop FilenameError, Rpc, Rpc !! RpcError, Embed IO] r =>
  Bool ->
  Modification ->
  (Path Abs File -> Path Abs File -> Sem r ()) ->
  Sem r ()
relocate raw modi run = do
  (bufPath, destPath) <- pathsForMod raw modi
  run bufPath destPath

copyOrFail ::
  Members [Stop FilenameError, Embed IO] r =>
  Path Abs File ->
  Path Abs File ->
  Sem r ()
copyOrFail src dest =
  stopEitherWith copyFailed =<< tryAny (copyFile src dest)
  where
    copyFailed e =
      FilenameError.ActionFailed "move" [exon|Couldn't copy file: #{e}|]

moveFile ::
  Members [Stop FilenameError, DataLog HostError, Embed IO] r =>
  Path Abs File ->
  Path Abs File ->
  Sem r ()
moveFile src dest = do
  copyOrFail src dest
  leftA (reportError (Just "filename") . FilenameError.Remove) =<< tryAny (removeFile src)

move ::
  Members [Stop FilenameError, DataLog HostError, Rpc, Rpc !! RpcError, Embed IO] r =>
  Bool ->
  Modification ->
  Sem r ()
move raw modi = do
  relocate raw modi \ buf dest -> do
    writeBuffer "move"
    moveFile buf dest
    updateBuffer dest

copy ::
  Members [Stop FilenameError, Rpc, Rpc !! RpcError, Embed IO] r =>
  Bool ->
  Modification ->
  Sem r ()
copy raw modi =
  relocate raw modi \ src dest -> do
    copyOrFail src dest
    view <- vimCallFunction "winsaveview" []
    edit dest
    vimCallFunction "winrestview" [view]

proMove ::
  Members [DataLog HostError, Rpc !! RpcError, Embed IO] r =>
  Bang ->
  Text ->
  Handler r ()
proMove bang spec =
  mapHandlerError @FilenameError $ resumeHandlerError @Rpc do
    move raw =<< smartModification raw spec
  where
    raw =
      bang == Bang

proCopy ::
  Members [Rpc !! RpcError, Embed IO] r =>
  Bang ->
  Text ->
  Handler r ()
proCopy bang spec =
  mapHandlerError @FilenameError $ resumeHandlerError @Rpc do
    copy raw =<< smartModification raw spec
  where
    raw =
      bang == Bang

proRemove ::
  Members [Rpc !! RpcError, PersistPath !! PersistPathError, DataLog HostError, Embed IO] r =>
  Handler r ()
proRemove =
  mapHandlerError @FilenameError $ resumeHandlerError @Rpc $ resumeHandlerError @PersistPath do
    move False =<< trashModification
    buf <- vimGetCurrentBuffer
    resume_ @RpcError do
      wipeBuffer buf
