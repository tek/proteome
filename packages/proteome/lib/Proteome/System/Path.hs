module Proteome.System.Path where

import Path (Abs, File, Path, parseRelFile)
import Path.IO (findExecutable)

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError (NoSuchExecutable, NotInPath))

findExe ::
  Members [Stop GrepError, Embed IO] r =>
  Text ->
  Sem r (Path Abs File)
findExe exe = do
  path <- stopEitherAs parseError (parseRelFile (toString exe))
  stopNote notInPath =<< findExecutable path
  where
    parseError =
      GrepError.NoSuchExecutable exe
    notInPath =
      GrepError.NotInPath exe
