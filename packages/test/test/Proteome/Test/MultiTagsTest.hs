module Proteome.Test.MultiTagsTest where

import Path (reldir, relfile, toFilePath, (</>))
import Path.IO (doesFileExist)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (bufferGetOption, vimGetCurrentBuffer)
import qualified Ribosome.Settings as Settings

import Proteome.Add (proAdd)
import Proteome.BufEnter (bufEnter)
import Proteome.Data.AddOptions (AddOptions (AddOptions))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import qualified Proteome.Settings as Settings (projectConfig, tagsArgs, tagsCommand, tagsFork)
import Proteome.Tags (proTags)
import qualified Proteome.Test.Dirs as Dirs
import Proteome.Test.Project (cn, createTestProject, fn, tp)
import Proteome.Test.Run (proteomeTest)

test_multi :: UnitTest
test_multi =
  proteomeTest do
    projectsDir <- Test.tempDir [reldir|multi/projects|]
    Settings.update Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
    createTestProject tp fn
    createTestProject tp cn
    Settings.update Settings.tagsCommand "touch"
    Settings.update Settings.tagsArgs ".tags"
    Settings.update Settings.tagsFork False
    proAdd $ AddOptions fn tp (Just False)
    proAdd $ AddOptions cn tp (Just False)
    proTags
    bufEnter
    let ftags = projectsDir </> Dirs.hask </> Dirs.flag </> [relfile|.tags|]
    let ctags = projectsDir </> Dirs.hask </> Dirs.cil </> [relfile|.tags|]
    ftagsExists <- doesFileExist ftags
    ctagsExists <- doesFileExist ctags
    buf <- vimGetCurrentBuffer
    tags <- bufferGetOption buf "tags"
    ftagsExists === True
    ctagsExists === True
    toFilePath ftags <> "," <> toFilePath ctags === tags
