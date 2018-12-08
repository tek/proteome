module Ribosome.Test.File(
  tempDirIO,
  tempDir,
) where

import Control.Monad.IO.Class (liftIO)
import Neovim (Neovim)
import System.Directory (canonicalizePath, createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

testDir :: String -> IO FilePath
testDir prefix = canonicalizePath $ "test" </> prefix

-- raises exception if cwd is not the package root so we don't damage anything
tempDirIO :: String -> FilePath -> IO FilePath
tempDirIO prefix path = do
  base <- testDir prefix
  let dir = base </> "temp"
  removePathForcibly dir
  createDirectoryIfMissing False dir
  let absPath = dir </> path
  createDirectoryIfMissing True absPath
  return absPath

tempDir :: String -> FilePath -> Neovim e FilePath
tempDir prefix path =
  liftIO $ tempDirIO prefix path
