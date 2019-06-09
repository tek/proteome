{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module DiagSpec (htf_thisModulesTests) where

import Path (Abs, Dir, Path, absdir, toFilePath)
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import Config (vars)
import Project (ag, flag, fn, hask, idr, l, la, li, ti, tp)
import Proteome.Data.Env (Env, Proteome)
import qualified Proteome.Data.Env as Env (configLog, mainProject)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Diag (proDiag)
import Unit (specWithDef)

root :: Path Abs Dir
root = [absdir|/projects/flagellum|]

main :: Project
main = Project (DirProject fn (ProjectRoot root) (Just tp)) [ti] (Just l) [la, li]

confLog :: [Text]
confLog = ["/conf/project/haskell/flagellum.vim", "/conf/project_after/all.vim"]

target :: [Text]
target = [
  "Diagnostics",
  "",
  "Main project",
  "",
  "name: " <> flag,
  "root: " <> toText (toFilePath root),
  "type: " <> hask,
  "tags cmd: ctags -R --languages=agda,idris -f /projects/flagellum/.tags.tmp /projects/flagellum/",
  "types: " <> idr,
  "main language: " <> hask,
  "languages: " <> ag <> ", " <> idr,
  "",
  "loaded config files:"
  ] <> confLog

diagSpec :: Proteome ()
diagSpec = do
  setL @Env Env.mainProject main
  setL @Env Env.configLog confLog
  proDiag def
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
