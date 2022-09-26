module Proteome.Test.TagsTest where

import Path (Abs, Dir, Path, absdir, reldir, relfile, (</>))
import Path.IO (doesFileExist)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assert, assertJust, unitTest, (===), assertEq)
import Ribosome (pathText)
import Ribosome.Api (currentBufferPath, nvimSetCurrentDir, optionSetList, currentLine)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (promptInput)
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import qualified Ribosome.Settings as Settings
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.ProjectLang (ProjectLang (ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import qualified Proteome.Settings as Settings
import Proteome.Tags.Cycle (proNextTag)
import Proteome.Tags.Gen (proGenTags)
import Proteome.Tags.Mappings (TagsAction (Navigate))
import Proteome.Tags.Menu (tagsMenu)
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
    proGenTags
    let tagsFile = root </> [relfile|tags-idris|]
    assert =<< embed (doesFileExist tagsFile)

navLine :: TagsAction -> Int
navLine (Navigate _ l) =
  l

test_tagsMenuTaglist :: UnitTest
test_tagsMenuTaglist =
  proteomeTest do
    atomicModify' (#mainProject . #meta .~ main [absdir|/|])
    tagFile <- Test.fixturePath [relfile|tags/tags|]
    optionSetList "tags" [pathText tagFile]
    result <- promptInput [Mapping "<c-s>", Mapping "<c-s>", Update "aeson", Mapping "<cr>"] (tagsMenu (Just "encode"))
    Menu.Success 162 === (navLine <$> result)

test_tagsMenuStream :: UnitTest
test_tagsMenuStream =
  proteomeTest do
    atomicModify' (#mainProject . #meta .~ main [absdir|/|])
    tagFile <- Test.fixturePath [relfile|tags/tags|]
    optionSetList "tags" [pathText tagFile]
    result <- promptInput [Update "eitherDecodeStrictWith", Mapping "<cr>"] (tagsMenu Nothing)
    Menu.Success 414 === (navLine <$> result)

test_tagsCycle :: UnitTest
test_tagsCycle =
  proteomeTest do
    root <- Test.tempDir [reldir|tags/project|]
    file1 <- Test.fixturePath [relfile|tags/project/File1.hs|]
    file2 <- Test.fixturePath [relfile|tags/project/File2.hs|]
    file3 <- Test.fixturePath [relfile|tags/project/File3.hs|]
    nvimSetCurrentDir (pathText root)
    tagFile <- Test.fixturePath [relfile|tags/project/tags|]
    optionSetList "tags" [pathText tagFile]
    proNextTag "func"
    assertJust file1 =<< currentBufferPath
    assertEq 2 =<< currentLine
    proNextTag "func"
    assertJust file2 =<< currentBufferPath
    assertEq 2 =<< currentLine
    proNextTag "func"
    assertJust file3 =<< currentBufferPath
    assertEq 2 =<< currentLine
    proNextTag "func"
    assertJust file1 =<< currentBufferPath
    assertEq 2 =<< currentLine

test_tags :: TestTree
test_tags =
  testGroup "tags" [
    unitTest "generate tags" test_simpleTags,
    unitTest "tags menu prefiltered" test_tagsMenuTaglist,
    unitTest "cycle tags" test_tagsCycle
  ]
