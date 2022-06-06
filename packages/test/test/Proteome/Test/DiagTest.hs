module Proteome.Test.DiagTest where

import Control.Lens ((.~))
import Path (Abs, Dir, Path, absdir, toFilePath)
import Polysemy.Test (UnitTest, (===))
import Ribosome.Api.Buffer (currentBufferContent)

import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Diag (proDiag)
import Proteome.Test.Project (ag, flag, fn, hask, idr, l, la, li, ti, tp)
import Proteome.Test.Run (proteomeTest)

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

test_diag :: UnitTest
test_diag =
  proteomeTest do
    atomicModify' (#mainProject .~ main)
    atomicModify' (#configLog .~ confLog)
    proDiag
    content <- currentBufferContent
    target === content
