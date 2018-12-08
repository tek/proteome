module Ribosome.Test.Unit(
  unitSpec,
  tempDir,
  tempFile,
  uPrefix,
) where

import System.FilePath (takeDirectory, takeFileName, (</>))
import Neovim (Neovim)
import Ribosome.Data.Ribo (Ribo)
import Ribosome.Test.Embed (TestConfig(..), setupPluginEnv, unsafeEmbeddedSpec)
import qualified Ribosome.Test.File as F (tempDir)

uPrefix :: String
uPrefix = "u"

uSpec :: TestConfig -> Neovim env () -> Neovim env ()
uSpec conf spec = do
  setupPluginEnv conf
  spec

unitSpec :: TestConfig -> e -> Ribo e () -> IO ()
unitSpec =
  unsafeEmbeddedSpec uSpec

tempDir :: FilePath -> Neovim e FilePath
tempDir = F.tempDir uPrefix

tempFile :: FilePath -> Neovim e FilePath
tempFile file = do
  absDir <- tempDir $ takeDirectory file
  return $ absDir </> takeFileName file
