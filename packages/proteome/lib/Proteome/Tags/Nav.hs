module Proteome.Tags.Nav where

import Path (Abs, File, Path)
import Prelude hiding (tag)
import Ribosome (Buffer, Rpc)
import Ribosome.Api (bufferForFile, edit, nvimBufIsLoaded, nvimCommand, nvimGetCurrentWin, nvimWinSetBuf, setCursor)
import qualified Ribosome.Data.FileBuffer as FileBuffer

filterUnloaded ::
  Member Rpc r =>
  Buffer ->
  Sem r (Maybe Buffer)
filterUnloaded buffer =
  nvimBufIsLoaded buffer <&> \case
    True -> Just buffer
    False -> Nothing

loadOrEdit ::
  Member Rpc r =>
  Path Abs File ->
  Int ->
  Sem r Bool
loadOrEdit file line = do
  existingBuffer <- join <$> (traverse (filterUnloaded . FileBuffer.buffer) =<< bufferForFile file)
  window <- nvimGetCurrentWin
  maybe (edit file) (nvimWinSetBuf window) existingBuffer
  setCursor window line 0
  nvimCommand "normal! zv"
  nvimCommand "normal! zz"
  pure (isJust existingBuffer)
