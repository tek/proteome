module Proteome.Tags(
  proTags,
) where

import GHC.IO.Exception (ExitCode(..))
import Data.Maybe (maybeToList)
import System.Process (readProcessWithExitCode)
import System.Directory (setCurrentDirectory)
import Control.Monad.IO.Class (liftIO)
import Ribosome.Config.Settings (setting)
import Ribosome.Data.Ribo (riboInspect)
import Ribosome.Internal.IO (forkNeovim)
import Proteome.Data.Env (Env(mainProject))
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (Project (Project), ProjectLang, ProjectMetadata (DirProject))
import qualified Proteome.Settings as S (tagsCommand, tagsArgs, tagsFork)

formatArgs :: String -> [ProjectLang] -> String
formatArgs a _ = a

deleteTags :: Proteome ()
deleteTags = return ()

notifyError :: String -> Proteome ()
notifyError _ = return ()

executeTags :: FilePath -> String -> String -> Proteome ()
executeTags root cmd args = do
  liftIO $ setCurrentDirectory root
  deleteTags
  (exitcode, _, stderr) <- liftIO $ readProcessWithExitCode cmd [args] ""
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure _ -> notifyError stderr

regenerateTags :: FilePath -> [ProjectLang] -> Proteome ()
regenerateTags root langs = do
  cmd <- setting S.tagsCommand
  args <- setting S.tagsArgs
  let thunk = executeTags root cmd (formatArgs args langs)
  fork <- setting S.tagsFork
  _ <- if fork then forkNeovim thunk else thunk
  return ()

proTags :: Proteome ()
proTags = do
  main <- riboInspect mainProject
  case main of
    Project (DirProject _ root _) _ lang langs ->
      regenerateTags root (maybeToList lang ++ langs)
    _ -> return ()
  liftIO $ print main
