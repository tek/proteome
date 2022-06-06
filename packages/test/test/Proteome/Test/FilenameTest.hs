module Proteome.Test.FilenameTest where

import Exon (exon)
import Path (File, Path, Rel, parent, reldir, relfile, stripProperPrefix, (</>))
import Path.IO (createDirIfMissing, doesFileExist, listDir)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, UnitTest, assert, assertEq, evalMaybe, unitTest, (===))
import Ribosome (Bang (Bang, NoBang), Rpc)
import Ribosome.Api (nvimCommand)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Effect.PersistPath (PersistPath, persistSubPath)
import Ribosome (pathText)
import Ribosome.Test (resumeTestError)
import Test.Tasty (TestTree, testGroup)

import Proteome.Filename (proCopy, proMove, proRemove)
import Proteome.Test.Run (proteomeTest)

filenameTest ::
  Members [Rpc, Hedgehog IO, Test, Embed IO] r =>
  Path Rel File ->
  Path Rel File ->
  Bool ->
  (Text -> Sem r ()) ->
  Sem r ()
filenameTest origRel changedRel origExist cmd = do
  base <- Test.tempDir [reldir|rename|]
  let cwd = parent base
  nvimCommand [exon|cd #{pathText cwd}|]
  baseRel <- evalMaybe (pathText <$> stripProperPrefix cwd base)
  let
    initial = base </> origRel
    changed = base </> changedRel
  createDirIfMissing True (parent initial)
  edit initial
  nvimCommand "write"
  cmd baseRel
  nvimCommand "write"
  assertEq (pathText changed) =<< currentBufferName
  assert =<< doesFileExist changed
  assertEq origExist =<< doesFileExist initial

basic ::
  Members [Rpc, Hedgehog IO, Test, Embed IO] r =>
  Path Rel File ->
  Bool ->
  (Text -> Sem r ()) ->
  Sem r ()
basic =
  filenameTest [relfile|File.hs|]

test_moveRelDir :: UnitTest
test_moveRelDir =
  proteomeTest do
    basic [relfile|sub/dir/File.hs|] False \ b -> proMove NoBang [exon|#{b}/sub/dir/|]

test_moveRename :: UnitTest
test_moveRename =
  proteomeTest do
    basic [relfile|Changed.hs|] False \ _ -> proMove NoBang "Changed"

test_moveExt :: UnitTest
test_moveExt =
  proteomeTest do
    basic [relfile|Changed.md|] False \ _ -> proMove NoBang "Changed.md"

test_moveDot :: UnitTest
test_moveDot =
  proteomeTest do
    filenameTest [relfile|.conf.json|] [relfile|conf.json|] False \ _ -> proMove NoBang "conf.json"
    filenameTest [relfile|.conf.json|] [relfile|.conf.md|] False \ _ -> proMove NoBang ".conf.md"
    filenameTest [relfile|.conf.json|] [relfile|conf.md|] False \ _ -> proMove NoBang "conf.md"

test_moveNoDots :: UnitTest
test_moveNoDots =
  proteomeTest do
    filenameTest [relfile|conf.file.json|] [relfile|conf.json|] False \ _ ->
      proMove Bang "conf.json"

test_multiExt :: UnitTest
test_multiExt =
  proteomeTest do
    filenameTest [relfile|a.b.c.d|] [relfile|x.y.z.q|] False \ _ -> proMove NoBang "x.y.z.q"
    filenameTest [relfile|a.b.c.d|] [relfile|x.y.z.d|] False \ _ -> proMove NoBang "x.y.z"

test_renameExt :: UnitTest
test_renameExt =
  proteomeTest do
    filenameTest [relfile|a.b.c.d|] [relfile|a.b.z.q|] False \ _ -> proMove NoBang "*.z.q"
    basic [relfile|File.md|] False \ _ -> proMove NoBang "*.md"

test_renameDir :: UnitTest
test_renameDir =
  proteomeTest do
    filenameTest [relfile|dir1/dir2/file|] [relfile|dir3/dir2/file|] False \ _ -> proMove NoBang "^^dir3"

test_copyRename :: UnitTest
test_copyRename =
  proteomeTest do
    basic [relfile|Changed.hs|] True \ _ -> proCopy NoBang "Changed"

test_remove :: UnitTest
test_remove = do
  proteomeTest do
    trash <- resumeTestError @PersistPath (persistSubPath [reldir|proteome/trash|])
    base <- Test.tempDir [reldir|rename|]
    let
      initial = base </> [relfile|File.hs|]
    edit initial
    nvimCommand "write"
    proRemove
    ("" ===) =<< currentBufferName
    (True ===) . not =<< doesFileExist initial
    trashFiles <- listDir trash
    1 === length trashFiles

test_filename :: TestTree
test_filename =
  testGroup "filenames" [
    unitTest "move a file into a relative dir" test_moveRelDir,
    unitTest "rename a file" test_moveRename,
    unitTest "rename a file with extension" test_moveExt,
    unitTest "rename a file with leading dot" test_moveDot,
    unitTest "rename a file without dot analysis" test_moveNoDots,
    unitTest "rename a file with three extensions" test_multiExt,
    unitTest "rename an extension" test_renameExt,
    unitTest "rename a containing directory" test_renameDir,
    unitTest "copy a file" test_copyRename,
    unitTest "remove a file" test_remove
  ]
