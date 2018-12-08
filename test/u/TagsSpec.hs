{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TagsSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (set)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Test.Framework
import Ribosome.Config.Settings (updateSetting)
import Ribosome.Data.Ribo (riboModify)
import Ribosome.Test.Unit (tempDir)
-- import Ribosome.Test.Exists (sleep)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Project (
  Project(Project),
  ProjectName(..),
  ProjectType(..),
  ProjectMetadata(DirProject),
  )
import qualified Proteome.Settings as S (tagsCommand, tagsArgs, tagsFork)
import Proteome.Tags (proTags)
import Proteome.Test.Unit (specWithDef)
import Config (vars)

main :: FilePath -> Project
main root = Project (DirProject (ProjectName "flagellum") root (Just (ProjectType "haskell"))) [] Nothing []

tagsSpec :: Proteome ()
tagsSpec = do
  root <- tempDir "projects/haskell/flagellum"
  riboModify $ set _mainProject (main root)
  updateSetting S.tagsCommand "touch"
  updateSetting S.tagsArgs "tags"
  updateSetting S.tagsFork False
  proTags
  let tagsFile = root </> "tags"
  exists <- liftIO $ doesFileExist tagsFile
  liftIO $ assertBool exists

test_tags :: IO ()
test_tags = vars >>= specWithDef tagsSpec
