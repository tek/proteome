module Proteome.Test.FilenameTest where

import Neovim (CommandArguments (bang))
import Hedgehog ((===))
import Path (Dir, File, Path, Rel, parseAbsDir, reldir, relfile, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesFileExist, getCurrentDir, listDir)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (tempDir)
import Test.Tasty (TestTree, testGroup)

import Proteome.Data.Env (Proteome)
import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Path (pathText)
import Proteome.Test.Unit (ProteomeTest, tmuxTest)

filenameTest ::
  Path Rel File ->
  Path Rel File ->
  Bool ->
  (Text -> Proteome ()) ->
  ProteomeTest ()
filenameTest origRel changedRel origExist cmd = do
  cwd <- getCurrentDir
  base <- parseAbsDir =<< tempDir "rename"
  baseRel <- pathText <$> stripProperPrefix cwd base
  let
    initial = base </> origRel
    changed = base </> changedRel
  edit (toFilePath initial)
  vimCommand "write"
  lift (cmd baseRel)
  vimCommand "write"
  (pathText changed ===) =<< currentBufferName
  (True ===) =<< doesFileExist changed
  (origExist ===) =<< doesFileExist initial

basic ::
  Path Rel File ->
  Bool ->
  (Text -> Proteome ()) ->
  ProteomeTest ()
basic =
  filenameTest [relfile|File.hs|]

moveRelDirTest :: ProteomeTest ()
moveRelDirTest = do
  basic [relfile|sub/dir/File.hs|] False \ b -> proMove def [text|#{b}/sub/dir/|]

test_moveRelDir :: UnitTest
test_moveRelDir =
  tmuxTest moveRelDirTest

moveRenameTest :: ProteomeTest ()
moveRenameTest =
  basic [relfile|Changed.hs|] False \ _ -> proMove def "Changed"

test_moveRename :: UnitTest
test_moveRename =
  tmuxTest moveRenameTest

moveExtTest :: ProteomeTest ()
moveExtTest =
  basic [relfile|Changed.md|] False \ _ -> proMove def "Changed.md"

test_moveExt :: UnitTest
test_moveExt =
  tmuxTest moveExtTest

moveDotTest :: ProteomeTest ()
moveDotTest = do
  filenameTest [relfile|.conf.json|] [relfile|conf.json|] False \ _ -> proMove def "conf.json"
  filenameTest [relfile|.conf.json|] [relfile|.conf.md|] False \ _ -> proMove def ".conf.md"
  filenameTest [relfile|.conf.json|] [relfile|conf.md|] False \ _ -> proMove def "conf.md"

test_moveDot :: UnitTest
test_moveDot =
  tmuxTest moveDotTest

moveNoDotsTest :: ProteomeTest ()
moveNoDotsTest =
  filenameTest [relfile|conf.file.json|] [relfile|conf.json|] False \ _ ->
    proMove def { bang = Just True } "conf.json"

test_moveNoDots :: UnitTest
test_moveNoDots =
  tmuxTest moveNoDotsTest

multiExtTest :: ProteomeTest ()
multiExtTest = do
  filenameTest [relfile|a.b.c.d|] [relfile|x.y.z.q|] False \ _ -> proMove def "x.y.z.q"
  filenameTest [relfile|a.b.c.d|] [relfile|x.y.z.d|] False \ _ -> proMove def "x.y.z"

test_multiExt :: UnitTest
test_multiExt =
  tmuxTest multiExtTest

copyRenameTest :: ProteomeTest ()
copyRenameTest = do
  basic [relfile|Changed.hs|] True \ _ -> proCopy def "Changed"

test_copyRename :: UnitTest
test_copyRename =
  tmuxTest copyRenameTest

removeTest :: ProteomeTest ()
removeTest = do
  persistDir <- parseAbsDir =<< tempDir "rename/persist"
  updateSetting persistenceDir (toFilePath persistDir)
  base <- parseAbsDir =<< tempDir "rename"
  let
    initial = base </> [relfile|File.hs|]
  edit (toFilePath initial)
  vimCommand "write"
  lift proRemove
  ("" ===) =<< currentBufferName
  (True ===) . not =<< doesFileExist initial
  trash <- listDir (persistDir </> [reldir|proteome/trash|])
  1 === length trash

test_remove :: UnitTest
test_remove = do
  tmuxTest removeTest

test_filename :: TestTree
test_filename =
  testGroup "filenames" [
    unitTest "move a file into a relative dir" test_moveRelDir,
    unitTest "rename a file" test_moveRename,
    unitTest "rename a file with extension" test_moveExt,
    unitTest "rename a file with leading dot" test_moveDot,
    unitTest "rename a file without dot analysis" test_moveNoDots,
    unitTest "rename a file with three extensions" test_multiExt,
    unitTest "copy a file" test_copyRename,
    unitTest "remove a file" test_remove
  ]
