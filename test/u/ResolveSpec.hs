{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ResolveSpec(
  htf_thisModulesTests
) where

import Data.Map.Strict (Map)
import qualified Data.Map as Map (fromList)
import Data.Default.Class (Default(def))
import Test.Framework
import Ribosome.File (canonicalPaths)
import Proteome.Config (ProjectConfig(ProjectConfig))
import Proteome.Data.Project (
  Project(Project),
  ProjectRoot(ProjectRoot),
  ProjectType(ProjectType),
  ProjectName(ProjectName),
  ProjectLang(ProjectLang),
  ProjectMetadata(DirProject),
  )
import Proteome.Project.Resolve (resolveProject)

paths :: [FilePath]
paths = [
  "~/../test/dir",
  "~"
  ]

test_canonicalPaths :: IO ()
test_canonicalPaths = do
  canon <- canonicalPaths paths
  assertNotEqual canon paths

root :: ProjectRoot
root = ProjectRoot "/projects/haskell/flagellum"

name :: ProjectName
name = ProjectName "flagellum"

tpe :: ProjectType
tpe = ProjectType "haskell"

tpe1 :: ProjectType
tpe1 = ProjectType "idris"

tpe2 :: ProjectType
tpe2 = ProjectType "agda"

lang :: ProjectLang
lang = ProjectLang "haskell"

lang1 :: ProjectLang
lang1 = ProjectLang "idris"

lang2 :: ProjectLang
lang2 = ProjectLang "agda"

typeMap :: Map ProjectType [ProjectType]
typeMap = Map.fromList [(tpe, [tpe1, tpe2])]

config :: ProjectConfig
config = ProjectConfig def typeMap (Map.fromList [(tpe, lang)]) (Map.fromList [(lang, [lang1, lang2])])

targetProject :: Project
targetProject = Project (DirProject name root (Just tpe)) [tpe1, tpe2] (Just lang) [lang1, lang2]

test_typeMap :: IO ()
test_typeMap = do
  project <- resolveProject ["/projects"] [] config (Just root) name (Just tpe)
  assertEqual targetProject project
