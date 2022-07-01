module Proteome.Test.TagsTest where

import Control.Lens ((.~), (?~))
import Path (Abs, Dir, Path, reldir, relfile, (</>))
import Path.IO (doesFileExist)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assert)
import qualified Ribosome.Settings as Settings

import Proteome.Data.ProjectLang (ProjectLang (ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import qualified Proteome.Settings as Settings
import Proteome.Tags (proTags)
import Proteome.Test.Run (proteomeTest)

main :: Path Abs Dir -> ProjectMetadata
main root = DirProject (ProjectName "flagellum") (ProjectRoot root) (Just (ProjectType "haskell"))

test_simpleTags :: UnitTest
test_simpleTags =
  proteomeTest do
    root <- Test.tempDir [reldir|projects/haskell/flagellum|]
    atomicModify' (#mainProject . #meta .~ main root)
    atomicModify' (#mainProject . #lang ?~ ProjectLang "idris")
    Settings.update Settings.tagsCommand "touch"
    Settings.update Settings.tagsArgs "tags-{langsComma}"
    Settings.update Settings.tagsFork False
    proTags
    let tagsFile = root </> [relfile|tags-idris|]
    assert =<< embed (doesFileExist tagsFile)
