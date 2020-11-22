{-# OPTIONS_GHC -F -pgmF htfpp #-}

module FilenameSpec (htf_thisModulesTests) where

import Path (File, Path, Rel, parseAbsDir, reldir, relfile, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesFileExist, getCurrentDir, listDir)
import Proteome.Filename (proRemove)
import Ribosome.Api.Buffer (currentBufferName, edit)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (persistenceDir)
import Ribosome.Nvim.Api.IO (vimCommand)
import Test.Framework

import Proteome.Data.Env (Proteome)
import Proteome.Filename (proCopy, proMove)
import Proteome.Path (pathText)
import Unit (tmuxSpec)

filenameSpec ::
  (Text -> Proteome ()) ->
  Path Rel File ->
  Proteome Bool
filenameSpec cmd changedRel = do
  cwd <- getCurrentDir
  base <- parseAbsDir =<< tempDir "rename"
  baseRel <- pathText <$> stripProperPrefix cwd base
  let
    initial = base </> [relfile|File.hs|]
    changed = base </> changedRel
  edit (toFilePath initial)
  vimCommand "write"
  cmd baseRel
  vimCommand "write"
  gassertBool =<< doesFileExist changed
  gassertEqual (pathText changed) =<< currentBufferName
  doesFileExist initial

moveRelDirSpec :: Proteome ()
moveRelDirSpec = do
  initialExists <- filenameSpec (\ b -> proMove [qt|${b}/sub/dir/|]) [relfile|sub/dir/File.hs|]
  gassertBool (not initialExists)

test_moveRelDir :: IO ()
test_moveRelDir =
  tmuxSpec moveRelDirSpec

moveRenameSpec :: Proteome ()
moveRenameSpec = do
  initialExists <- filenameSpec (\ _ -> proMove "Changed") [relfile|Changed.hs|]
  gassertBool (not initialExists)

test_moveRename :: IO ()
test_moveRename =
  tmuxSpec moveRenameSpec

copyRenameSpec :: Proteome ()
copyRenameSpec = do
  initialExists <- filenameSpec (\ _ -> proCopy "Changed") [relfile|Changed.hs|]
  gassertBool initialExists

test_copyRename :: IO ()
test_copyRename =
  tmuxSpec copyRenameSpec

removeSpec :: Proteome ()
removeSpec = do
  persistDir <- parseAbsDir =<< tempDir "rename/persist"
  updateSetting persistenceDir (toFilePath persistDir)
  base <- parseAbsDir =<< tempDir "rename"
  let
    initial = base </> [relfile|File.hs|]
  edit (toFilePath initial)
  vimCommand "write"
  proRemove
  gassertEqual "" =<< currentBufferName
  gassertBool . not =<< doesFileExist initial
  trash <- listDir (persistDir </> [reldir|proteome/trash|])
  gassertEqual 1 (length trash)

test_remove :: IO ()
test_remove = do
  tmuxSpec removeSpec
