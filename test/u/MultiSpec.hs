{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MultiSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Test.Framework
import Neovim (fromObject', vim_get_current_buffer', buffer_get_option')
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Unit (tempDir)
import Proteome.Data.Proteome (Proteome)
import Proteome.Add (proAdd)
import Proteome.Data.AddOptions (AddOptions(AddOptions))
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Tags (proTags)
import Proteome.Test.Unit (specWithDef)
import Proteome.BufEnter (bufEnter)
import qualified Proteome.Settings as PS (tagsCommand, tagsArgs, tagsFork)
import Config (vars)
import Project (flag, cil, hask, cn, fn, tp, createTestProject)

multiSpec :: Proteome ()
multiSpec = do
  projectsDir <- tempDir "multi/projects"
  updateSetting S.projectBaseDirs [projectsDir]
  createTestProject tp fn
  createTestProject tp cn
  updateSetting PS.tagsCommand "touch"
  updateSetting PS.tagsArgs ".tags"
  updateSetting PS.tagsFork False
  proAdd $ AddOptions fn tp
  proAdd $ AddOptions cn tp
  proTags
  bufEnter
  let ftags = projectsDir </> hask </> flag </> ".tags"
  let ctags = projectsDir </> hask </> cil </> ".tags"
  ftagsExists <- liftIO $ doesFileExist ftags
  ctagsExists <- liftIO $ doesFileExist ctags
  buf <- vim_get_current_buffer'
  tags <- buffer_get_option' buf "tags" >>= fromObject'
  liftIO $ assertEqual ftagsExists True
  liftIO $ assertEqual ctagsExists True
  liftIO $ assertEqual (ftags ++ "," ++ ctags) tags

test_multi :: IO ()
test_multi = vars >>= specWithDef multiSpec
