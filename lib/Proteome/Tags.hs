{-# LANGUAGE OverloadedStrings #-}

module Proteome.Tags(
  proTags,
) where

import GHC.IO.Exception (ExitCode(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Lens (over)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map (adjust)
import Data.Maybe (maybeToList)
import Data.String.Utils (replace)
import System.Process (readCreateProcessWithExitCode)
import qualified System.Process as Proc (proc, CreateProcess(cwd))
import System.FilePath ((</>))
import System.Directory (doesFileExist, removePathForcibly)
import Ribosome.Config.Setting (setting)
import qualified Ribosome.Data.Ribo as Ribo (inspect, modify)
import Ribosome.Data.Errors (Errors(Errors), Error(Error), ComponentName(ComponentName))
import Ribosome.Internal.IO (forkNeovim)
import Proteome.Data.Env (Env(mainProject))
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

replaceFormatItem :: String -> (String, String) -> String
replaceFormatItem original (placeholder, replacement) =
  replace ("{" ++ placeholder ++ "}") replacement original

formatTagsArgs :: [ProjectLang] -> ProjectRoot -> FilePath -> String -> String
formatTagsArgs langs (ProjectRoot root) fileName formatString =
  foldl replaceFormatItem formatString formats
  where
    formats = [
      ("langsComma", intercalate "," $ fmap (\(ProjectLang l) -> l) langs),
      ("tagFile", fileName),
      ("root", root)
      ]

deleteTags :: ProjectRoot -> Proteome ()
deleteTags (ProjectRoot root) = do
  name <- setting S.tagsFileName
  let path = root </> name
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removePathForcibly path

storeError :: ComponentName -> [String] -> Errors -> Errors
storeError name msg (Errors errors) =
  Errors (Map.adjust (err:) name errors)
  where
    err = Error time msg
    time = 0

notifyError :: String -> Proteome ()
notifyError e =
  Ribo.modify $ over Env._errors (storeError (ComponentName "ctags") [e])

executeTags :: ProjectRoot -> String -> String -> Proteome ()
executeTags (ProjectRoot root) cmd args = do
  deleteTags (ProjectRoot root)
  (exitcode, _, stderr) <- liftIO $ readCreateProcessWithExitCode ((Proc.proc cmd [args]) { Proc.cwd = (Just root) }) ""
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure _ -> notifyError stderr

regenerateTags :: ProjectRoot -> [ProjectLang] -> Proteome ()
regenerateTags root langs = do
  cmd <- setting S.tagsCommand
  args <- setting S.tagsArgs
  fileName <- setting S.tagsFileName
  let thunk = executeTags root cmd (formatTagsArgs langs root fileName args)
  fork <- setting S.tagsFork
  _ <- if fork then forkNeovim thunk else thunk
  return ()

proTags :: Proteome ()
proTags = do
  main <- Ribo.inspect mainProject
  case main of
    Project (DirProject _ root tpe) _ lang langs ->
      regenerateTags root (maybeToList (langOrType lang tpe) ++ langs)
    _ -> return ()
