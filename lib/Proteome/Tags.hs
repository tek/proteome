{-# LANGUAGE OverloadedStrings #-}

module Proteome.Tags(
  proTags,
  tagsCommand,
) where

import GHC.IO.Exception (ExitCode(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Lens (over)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map (adjust)
import Data.Maybe (maybeToList)
import Data.String.Utils (replace)
import System.Process (readCreateProcessWithExitCode)
import qualified System.Process as Proc (proc, CreateProcess(cwd))
import System.FilePath ((</>))
import System.Directory (doesFileExist, removePathForcibly, renameFile)
import UnliftIO (tryIO)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Ribo (lockOrSkip)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Data.Errors (Errors(Errors), Error(Error), ComponentName(ComponentName))
import Ribosome.Internal.IO (forkNeovim)
import Proteome.Data.Env (Env(mainProject, projects))
import qualified Proteome.Data.Env as Env (_errors)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (
  Project (Project),
  ProjectLang(ProjectLang),
  ProjectRoot(ProjectRoot),
  ProjectMetadata (DirProject),
  langOrType,
  )
import qualified Proteome.Settings as S (tagsCommand, tagsArgs, tagsFork, tagsFileName)
import qualified Proteome.Log as Log

replaceFormatItem :: String -> (String, String) -> String
replaceFormatItem original (placeholder, replacement) =
  replace ("{" ++ placeholder ++ "}") replacement original

formatTagsArgs :: [ProjectLang] -> ProjectRoot -> FilePath -> String -> String
formatTagsArgs langs (ProjectRoot root) fileName formatString =
  foldl replaceFormatItem formatString formats
  where
    formats = [
      ("langsComma", intercalate "," $ fmap (\(ProjectLang l) -> l) langs),
      ("tagFile", root </> fileName),
      ("root", root)
      ]

tempname :: String -> String
tempname name = name ++ ".tmp"

deleteTags :: ProjectRoot -> Proteome ()
deleteTags (ProjectRoot root) = do
  name <- setting S.tagsFileName
  let path = root </> tempname name
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removePathForcibly path

replaceTags :: ProjectRoot -> Proteome ()
replaceTags (ProjectRoot root) = do
  name <- setting S.tagsFileName
  let temppath = root </> tempname name
  let path = root </> name
  _ <- liftIO $ tryIO $ renameFile temppath path
  return ()

storeError :: ComponentName -> [String] -> Errors -> Errors
storeError name msg (Errors errors) =
  Errors (Map.adjust (err:) name errors)
  where
    err = Error time msg
    time = 0

notifyError :: String -> Proteome ()
notifyError e = do
  Log.info $ "tags failed: " ++ e
  Ribo.modify $ over Env._errors (storeError (ComponentName "ctags") [e])

tagsProcess :: ProjectRoot -> String -> String -> IO (ExitCode, String, String)
tagsProcess (ProjectRoot root) cmd args =
  readCreateProcessWithExitCode (Proc.proc cmd (words args)) { Proc.cwd = Just root } ""

executeTags :: ProjectRoot -> String -> String -> Proteome ()
executeTags root@(ProjectRoot rootS) cmd args = do
  deleteTags root
  Log.debugS $ "executing tags: `" ++ cmd ++ " " ++ args ++ "` in directory " ++ rootS
  (exitcode, _, stderr) <- liftIO $ tagsProcess root cmd args
  case exitcode of
    ExitSuccess -> replaceTags root
    ExitFailure _ -> notifyError stderr

tagsCommand :: ProjectRoot -> [ProjectLang] -> Proteome (String, String)
tagsCommand root langs = do
  cmd <- setting S.tagsCommand
  args <- setting S.tagsArgs
  fileName <- setting S.tagsFileName
  return (cmd, formatTagsArgs langs root (tempname fileName) args)

regenerateTags :: ProjectRoot -> [ProjectLang] -> Proteome ()
regenerateTags root langs = do
  (cmd, args) <- tagsCommand root langs
  let thunk = executeTags root cmd args
  fork <- setting S.tagsFork
  _ <- if fork then forkNeovim thunk else thunk
  return ()

projectTags :: Project -> Proteome ()
projectTags (Project (DirProject _ root tpe) _ lang langs) =
  regenerateTags root (maybeToList (langOrType lang tpe) ++ langs)
projectTags _ = return ()

proTags :: Proteome ()
proTags = do
  main <- Ribo.inspect mainProject
  extra <- Ribo.inspect projects
  lockOrSkip "tags" $ traverse_ projectTags (main : extra)
