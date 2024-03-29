module Proteome.Tags.Gen where

import Conc (Lock, lockOrSkip_)
import qualified Data.List as List
import Data.Sequence ((|>))
import qualified Data.Text as Text (intercalate, replace)
import Exon (exon)
import GHC.IO.Exception (ExitCode (..))
import qualified Log
import Log (Severity (Warn))
import Path (File, Path, Rel, addExtension, toFilePath, (</>))
import Path.IO (doesFileExist, removeFile, renameFile)
import Polysemy.Process (SysProcConf, SystemProcess, SystemProcessScopeError, interpretSystemProcessNativeSingle)
import qualified Polysemy.Process.SystemProcess as Process
import Ribosome (
  Handler,
  LogReport,
  Report (Report),
  Reports,
  SettingError,
  Settings,
  mapReport,
  reportStop,
  resumeReport,
  )
import Ribosome.Report (storeReport)
import qualified Ribosome.Settings as Settings
import System.Process.Typed (proc, setWorkingDir)

import Proteome.Data.Env (Env)
import qualified Proteome.Data.Env as Env (mainProject, projects)
import Proteome.Data.Project (Project (Project), langOrType)
import Proteome.Data.ProjectLang (ProjectLang (unProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import qualified Proteome.Data.TagsError as TagsError
import Proteome.Data.TagsError (TagsError)
import qualified Proteome.Settings as Settings

data TagsLock =
  TagsLock
  deriving stock (Eq, Show)

replaceFormatItem :: Text -> (Text, Text) -> Text
replaceFormatItem original (placeholder, replacement) =
  Text.replace ("{" <> placeholder <> "}") replacement original

formatTagsArgs ::
  [ProjectLang] ->
  ProjectRoot ->
  Path Rel File ->
  Text ->
  Text
formatTagsArgs langs (ProjectRoot root) fileName formatString =
  foldl' @[] replaceFormatItem formatString formats
  where
    formats = [
      ("langsComma", Text.intercalate "," (fmap (.unProjectLang) langs)),
      ("tagFile", toText . toFilePath $ root </> fileName),
      ("root", toText . toFilePath $ root)
      ]

tempname ::
  Member (Stop TagsError) r =>
  Path Rel File ->
  Sem r (Path Rel File)
tempname name =
  stopEitherWith (const err) (addExtension ".tmp" name)
  where
    err =
      TagsError.TempName

deleteTempTags ::
  Members [Settings !! SettingError, Stop TagsError, Embed IO] r =>
  ProjectRoot ->
  Sem r ()
deleteTempTags (ProjectRoot root) = do
  name <- resumeHoist TagsError.Setting (Settings.get Settings.tagsFileName)
  path <- (root </>) <$> tempname name
  whenM (doesFileExist path) do
    tryAny_ (removeFile path)

replaceTags ::
  Members [Settings !! SettingError, Stop TagsError, Embed IO] r =>
  ProjectRoot ->
  Sem r ()
replaceTags (ProjectRoot root) = do
  name <- resumeHoist TagsError.Setting (Settings.get Settings.tagsFileName)
  temppath <- (root </>) <$> tempname name
  whenM (doesFileExist temppath) do
    stopTryIOError TagsError.RenameTags (renameFile temppath (root </> name))

notifyError ::
  Member Reports r =>
  [Text] ->
  Sem r ()
notifyError out =
  storeReport "tags" (Report "tag generation failed" ("tag subprocess failed: " : out) Warn)

tagsCommand ::
  Members [Settings, Stop TagsError] r =>
  ProjectRoot ->
  [ProjectLang] ->
  Sem r (Text, Text)
tagsCommand root langs = do
  cmd <- Settings.get Settings.tagsCommand
  args <- Settings.get Settings.tagsArgs
  fileName <- Settings.get Settings.tagsFileName
  tmp <- tempname fileName
  pure (cmd, formatTagsArgs langs root tmp args)

tagsProcess ::
  Members [Settings !! SettingError, Stop TagsError, Log] r =>
  ProjectRoot ->
  [ProjectLang] ->
  Sem r SysProcConf
tagsProcess projectRoot@(ProjectRoot root) langs = do
  (cmd, args) <- resumeHoist TagsError.Setting (tagsCommand projectRoot langs)
  Log.debug [exon|executing tags: `#{cmd} #{args}` in directory #{show root}|]
  pure (setWorkingDir (toFilePath root) (proc (toString cmd) (List.words (toString args))))

readStderr ::
  Member (SystemProcess !! err) r =>
  Sem r [Text]
readStderr =
  spin mempty
  where
    spin buf =
      resumeEither Process.readStderr >>= \case
        Right l -> spin (buf |> decodeUtf8 l)
        Left _ -> pure (toList buf)

-- TODO add timeout to kill the process
executeTags ::
  Members [Settings !! SettingError, Reports, Stop TagsError, Log, Resource, Embed IO] r =>
  ProjectRoot ->
  [ProjectLang] ->
  Sem r ()
executeTags projectRoot langs = do
  deleteTempTags projectRoot
  procConf <- tagsProcess projectRoot langs
  mapStop @SystemProcessScopeError (TagsError.Process . show) $ interpretSystemProcessNativeSingle procConf do
    resumeHoist (TagsError.Process . show) Process.wait >>= \case
      ExitSuccess -> do
        Log.debug "success"
        replaceTags projectRoot
      ExitFailure _ -> do
        Log.debug "failure"
        notifyError =<< readStderr

projectTags ::
  Members [Settings !! SettingError, Settings, Reports, Stop TagsError, Log, Resource, Embed IO] r =>
  Project ->
  Sem r ()
projectTags (Project (DirProject _ root tpe) _ lang langs) =
  executeTags root (maybeToList (langOrType lang tpe) <> langs)
projectTags _ =
  unit

execution ::
  Members [Settings !! SettingError, DataLog LogReport, Async, Stop Report] r =>
  Bool ->
  Sem (Stop TagsError : r) () ->
  Sem r ()
execution = \case
  True ->
    void . async . reportStop
  False ->
    mapReport

proGenTags ::
  Members [AtomicState Env, Settings !! SettingError, DataLog LogReport, Lock @@ TagsLock, Reports] r =>
  Members [Log, Resource, Async, Embed IO] r =>
  Handler r ()
proGenTags =
  resumeReport @Settings $ whenM (Settings.get Settings.tagsEnable) do
    main <- atomicGets (.mainProject)
    extra <- atomicGets (.projects)
    fork <- Settings.get Settings.tagsFork
    execution fork $ tag $ lockOrSkip_ do
      res <- sequenceConcurrently (runStop . projectTags <$> main : extra)
      traverse (void . stopEither . fromMaybe unit) res
