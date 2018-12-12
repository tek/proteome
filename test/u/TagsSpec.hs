{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TagsSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (set)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Test.Framework
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Ribosome.Test.Unit (tempDir)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Project (
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectLang(..),
  ProjectMetadata(DirProject),
  _meta,
  _lang,
  )
import qualified Proteome.Settings as S (tagsCommand, tagsArgs, tagsFork)
import Proteome.Tags (proTags)
import Proteome.Test.Unit (specWithDef)
import Config (vars)

main :: FilePath -> ProjectMetadata
main root = DirProject (ProjectName "flagellum") (ProjectRoot root) (Just (ProjectType "haskell"))

tagsSpec :: Proteome ()
tagsSpec = do
  root <- tempDir "projects/haskell/flagellum"
  Ribo.modify $ set (_mainProject._meta) (main root)
  Ribo.modify $ set (_mainProject._lang) (Just (ProjectLang "idris"))
  updateSetting S.tagsCommand "touch"
  updateSetting S.tagsArgs "tags-{langsComma}"
  updateSetting S.tagsFork False
  proTags
  let tagsFile = root </> "tags-idris"
  exists <- liftIO $ doesFileExist tagsFile
  liftIO $ assertBool exists

test_tags :: IO ()
test_tags = vars >>= specWithDef tagsSpec
