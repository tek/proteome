{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module TagsSpec (htf_thisModulesTests) where

import Path (Abs, Dir, Path, parseAbsDir, relfile, (</>))
import Path.IO (doesFileExist)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Unit (tempDir)
import Test.Framework

import Config (vars)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (mainProject)
import qualified Proteome.Data.Project as Project (lang, meta)
import Proteome.Data.ProjectLang (ProjectLang(ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectName (ProjectName(ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType(ProjectType))
import qualified Proteome.Settings as S (tagsArgs, tagsCommand, tagsFork)
import Proteome.Tags (proTags)
import Unit (specWithDef)

main :: Path Abs Dir -> ProjectMetadata
main root = DirProject (ProjectName "flagellum") (ProjectRoot root) (Just (ProjectType "haskell"))

tagsSpec :: Proteome ()
tagsSpec = do
  root <- parseAbsDir =<< tempDir "projects/haskell/flagellum"
  setL @Env (Env.mainProject . Project.meta) (main root)
  setL @Env (Env.mainProject . Project.lang) (Just (ProjectLang "idris"))
  updateSetting S.tagsCommand "touch"
  updateSetting S.tagsArgs "tags-{langsComma}"
  updateSetting S.tagsFork False
  proTags
  let tagsFile = root </> [relfile|tags-idris|]
  exists <- liftIO $ doesFileExist tagsFile
  liftIO $ assertBool exists

test_tags :: IO ()
test_tags = vars >>= specWithDef tagsSpec
