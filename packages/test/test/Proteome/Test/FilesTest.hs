module Proteome.Test.FilesTest where

import Control.Lens (view)
import Control.Monad.Catch (MonadThrow)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Set as Set (fromList)
import Hedgehog ((===))
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  isProperPrefixOf,
  parseAbsDir,
  parseAbsFile,
  reldir,
  relfile,
  toFilePath,
  (</>),
  )
import Path.IO (createDirIfMissing)
import Ribosome.Api.Buffer (currentBufferName)
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag (StartInsert), PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Run (noPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Tmux (tmuxTestDef)
import Ribosome.Test.Unit (fixture, tempDir)
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (serial)
import Test.Tasty (TestTree, testGroup)
import Text.RE.PCRE.Text (re)

import Proteome.Data.FilesConfig (FilesConfig (FilesConfig))
import Proteome.Files (filesWith)
import Proteome.Files.Source (files)
import qualified Proteome.Settings as Settings
import Proteome.Test.Unit (ProteomeTest, testDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInput chars' =
  PromptInput \ _ ->
    serial (Streamly.nilM (sleep 0.1)) (Streamly.fromList (PromptInputEvent.Character <$> chars'))

slowPromptInput ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadThrow m =>
  [Text] ->
  PromptInput m
slowPromptInput chars' =
  PromptInput \ _ ->
    serial (Streamly.nilM (sleep 0.1)) (Streamly.unfoldrM send chars')
  where
    send = \case
      c : cs ->
        Just (PromptInputEvent.Character c, cs) <$ sleep 0.1
      [] ->
        pure Nothing

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

paths :: Path Abs Dir -> NonEmpty (Path Abs Dir)
paths base =
  (base </> [reldir|dir1|]) :| [base </> [reldir|dir2|]]

editChars :: [Text]
editChars =
  ["k", "k", "k", "cr"]

filesEditTest :: ProteomeTest ()
filesEditTest = do
  updateSetting Settings.filesUseRg False
  dir <- parseAbsDir =<< fixture "files"
  filesWith (promptConfig editChars) dir (toText . toFilePath <$> NonEmpty.toList (paths dir))
  (True ===) . isProperPrefixOf dir =<< parseAbsFile . toString =<< currentBufferName

test_filesEdit :: UnitTest
test_filesEdit =
  testDef filesEditTest

conf :: FilesConfig
conf =
  FilesConfig False True [[re|b/c|]] [[re|/g/|/k/|]] []

test_filesExclude :: UnitTest
test_filesExclude = do
  dir <- parseAbsDir =<< fixture "files"
  (3 ===) . length =<< Streamly.toList (files conf (paths dir))

createChars :: [Text]
createChars =
  ["p", "tab", "t", "tab", "d", "tab", "f", "i", "l", "e", "c-y"]

filesCreateTest :: ProteomeTest ()
filesCreateTest = do
  updateSetting Settings.filesUseRg False
  base <- parseAbsDir =<< tempDir "files/create"
  let targetDir = base </> [reldir|path/to/dir|]
  createDirIfMissing True targetDir
  filesWith slowPromptConfig base [toText (toFilePath base)]
  (targetDir </> [relfile|file|] ===) =<< parseAbsFile . toString =<< currentBufferName
  where
    slowPromptConfig =
      PromptConfig (slowPromptInput createChars) basicTransition noPromptRenderer [StartInsert]

test_filesCreate :: UnitTest
test_filesCreate =
  tmuxTestDef filesCreateTest

filesMultiDirTest :: Bool -> UnitTest
filesMultiDirTest rg = do
  dir1 <- parseAbsDir =<< tempDir "files/multi/dir1"
  dir2 <- parseAbsDir =<< tempDir "files/multi/dir2"
  createDirIfMissing True dir1
  createDirIfMissing True dir2
  writeFile (toFilePath (dir1 </> [relfile|file1|])) "content"
  writeFile (toFilePath (dir2 </> [relfile|file2|])) "content"
  writeFile (toFilePath (dir1 </> [relfile|file.foo|])) "content"
  writeFile (toFilePath (dir1 </> [relfile|file.bar|])) "content"
  fs <- fmap (view MenuItem.truncated) <$> Streamly.toList (files conf' (dir1 :| [dir2]))
  target === Set.fromList fs
  where
    conf' =
      FilesConfig rg False [] [] ["*.foo", "bar"]
    target =
      Set.fromList [" * [dir1] file.bar", " * [dir1] file1", " * [dir2] file2"]

test_filesMultiDirNative :: UnitTest
test_filesMultiDirNative =
  filesMultiDirTest False

test_filesMultiDirRg :: UnitTest
test_filesMultiDirRg =
  filesMultiDirTest True

test_files :: TestTree
test_files =
  testGroup "files menu" [
    unitTest "edit a file" test_filesEdit,
    unitTest "exclude patterns" test_filesExclude,
    unitTest "create a file" test_filesCreate,
    unitTest "show dir prefix, native" test_filesMultiDirNative,
    unitTest "show dir prefix, rg" test_filesMultiDirRg
  ]
