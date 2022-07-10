module Proteome.Test.PersistLoadTest where

import qualified Data.Text.IO as Text
import Exon (exon)
import Path (absdir, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust, (===))
import Ribosome (pathText)
import Ribosome.Api (bufferGetName, currentBufferPath, vimGetBuffers, vimSetOption)
import Ribosome.Effect.Persist (Persist)
import Ribosome.Effect.PersistPath (PersistPath)
import qualified Ribosome.Persist as Persist
import Ribosome.Test (resumeTestError)

import qualified Proteome.Data.Env as Env
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import qualified Proteome.PersistBuffers as PersistBuffers
import Proteome.PersistBuffers (loadBuffers)
import Proteome.Test.Run (interpretPersistTest, proteomeTest)

main :: ProjectMetadata
main = DirProject (ProjectName "flagellum") (ProjectRoot [absdir|/|]) (Just (ProjectType "haskell"))

buffersJson :: Text -> Text
buffersJson base =
  [exon|{"current":"#{base}/file2","buffers":["#{base}/file1","#{base}/file2","#{base}/file3"]}|]

test_loadPersistedBuffers :: UnitTest
test_loadPersistedBuffers =
  proteomeTest $ interpretPersistTest do
    vimSetOption "swapfile" False
    atomicModify' (#mainProject . #meta .~ main)
    persistBase <- resumeTestError @PersistPath Persist.persistRoot
    let persistDir = persistBase </> [reldir|buffers/haskell/flagellum|]
    createDirIfMissing True persistDir
    fixDir <- Test.fixturePath [reldir|persist/store|]
    embed (Text.writeFile (toFilePath (persistDir </> PersistBuffers.file)) (buffersJson (pathText fixDir)))
    resumeTestError @(Persist _) loadBuffers
    buffers <- vimGetBuffers
    3 === length buffers
    active <- currentBufferPath
    assertJust (fixDir </> [relfile|file2|]) active
    (target fixDir ===) =<< traverse bufferGetName =<< atomicGets Env.buffers
  where
    target fixDir =
      pathText . (fixDir </>) <$> [[relfile|file1|], [relfile|file2|], [relfile|file3|]]
