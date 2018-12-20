{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Lens (set)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Test.Framework
import Ribosome.Api.Buffer (currentBufferContent)
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Project (
  Project (Project),
  ProjectRoot(ProjectRoot),
  ProjectMetadata (DirProject),
  )
import Proteome.Data.Proteome
import Proteome.Test.Unit (specWithDef)
import Proteome.Diag (proDiag)
import Config (vars)
import Project (fn, tp, ti, la, l, li, flag, idr, ag, hask)

root :: String
root = "/projects/flagellum"

main :: Project
main =
  Project (DirProject fn (ProjectRoot root) (Just tp)) [ti] (Just l) [la, li]

target :: [String]
target = [
  "Diagnostics",
  "",
  "Main project:",
  "name: " ++ flag,
  "root: " ++ root,
  "type: " ++ hask,
  "types: " ++ idr,
  "main language: " ++ hask,
  "languages: " ++ ag ++ ", " ++ idr
  ]

diagSpec :: Proteome ()
diagSpec = do
  Ribo.modify $ set _mainProject main
  proDiag def
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
