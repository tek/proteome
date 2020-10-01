{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module MultiTagsSpec (htf_thisModulesTests) where

import Path (parseAbsDir, parseRelDir, relfile, toFilePath, (</>))
import Path.IO (doesFileExist)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (bufferGetOption, vimGetCurrentBuffer)
import Test.Framework

import Config (vars)
import Project (cil, cn, createTestProject, flag, fn, hask, tp)
import Proteome.Add (proAdd)
import Proteome.BufEnter (bufEnter)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import Proteome.Data.Env (Proteome)
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import qualified Proteome.Settings as Settings (projectConfig, tagsArgs, tagsCommand, tagsFork)
import Proteome.Tags (proTags)
import Unit (specWithDef)

multiSpec :: Proteome ()
multiSpec = do
  projectsDir <- parseAbsDir =<< tempDir "multi/projects"
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  createTestProject tp fn
  createTestProject tp cn
  updateSetting Settings.tagsCommand "touch"
  updateSetting Settings.tagsArgs ".tags"
  updateSetting Settings.tagsFork False
  proAdd $ AddOptions fn tp (Just False)
  proAdd $ AddOptions cn tp (Just False)
  proTags
  bufEnter
  haskPath <- parseRelDir (toString hask)
  flagPath <- parseRelDir (toString flag)
  cilPath <- parseRelDir (toString cil)
  let ftags = projectsDir </> haskPath </> flagPath </> [relfile|.tags|]
  let ctags = projectsDir </> haskPath </> cilPath </> [relfile|.tags|]
  ftagsExists <- doesFileExist ftags
  ctagsExists <- doesFileExist ctags
  buf <- vimGetCurrentBuffer
  tags <- bufferGetOption buf "tags"
  gassertEqual ftagsExists True
  gassertEqual ctagsExists True
  gassertEqual (toFilePath ftags <> "," <> toFilePath ctags) tags

test_multi :: IO ()
test_multi = vars >>= specWithDef multiSpec
