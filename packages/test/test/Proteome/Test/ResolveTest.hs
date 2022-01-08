module Proteome.Test.ResolveTest where

import qualified Data.Map as Map (fromList)
import Hedgehog ((/==), (===))
import Path (Abs, Dir, Path, absdir, parent, parseAbsDir)
import Ribosome.File (canonicalPaths)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Unit (fixture)
import Test.Tasty (TestTree, testGroup)

import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig, _projectTypes, _typeDirs, _typeMarkers))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType)
import Proteome.Project.Resolve (fromName, fromRoot)
import Proteome.Test.Project (fn, l, la, li, ta, ti, tp)
import Proteome.Test.Unit (testDef)

paths :: [FilePath]
paths = [
  "~/../test/dir",
  "~"
  ]

test_canonicalPaths :: UnitTest
test_canonicalPaths = do
  canon <- canonicalPaths paths
  canon /== paths

rootDir :: Path Abs Dir
rootDir =
  [absdir|/projects/haskell/flagellum|]

root :: ProjectRoot
root = ProjectRoot rootDir

typeMap :: Map ProjectType [ProjectType]
typeMap = Map.fromList [(tp, [ti, ta])]

config :: Path Abs Dir -> ProjectConfig
config base =
  ProjectConfig [base] def def typeMap def (Map.fromList [(tp, l)]) (Map.fromList [(l, [li, la])])

targetProject :: Path Abs Dir -> Project
targetProject dir =
  Project (DirProject fn (ProjectRoot dir) (Just tp)) [ti, ta] (Just l) [li, la]

simpleTarget :: Project
simpleTarget =
  Project (DirProject fn root (Just tp)) [] (Just l) []

test_typeMap :: UnitTest
test_typeMap =
  testDef @_ @(Ribo _ _) do
    dir <- parseAbsDir =<< fixture "projects/haskell/flagellum"
    project <- fromName [] (config (parent (parent dir))) fn (Just tp)
    targetProject dir === project

markerTarget :: ProjectRoot -> Project
markerTarget root' =
  Project (DirProject fn root' (Just tp)) [] (Just l) []

test_marker :: UnitTest
test_marker =
  testDef @_ @(Ribo _ _) do
    (ProjectRoot -> dir) <- parseAbsDir =<< fixture "projects/haskell/flagellum"
    project <- fromRoot [] def { _typeMarkers = defaultTypeMarkers } dir
    markerTarget dir === project

test_typeDirs :: UnitTest
test_typeDirs =
  testDef @_ @(Ribo _ _) do
    project <- fromRoot [] def { _typeDirs = [("haskell", [parent rootDir])] } root
    simpleTarget === project

test_projectTypes :: UnitTest
test_projectTypes =
  testDef @_ @(Ribo _ _) do
    project <- fromRoot [] def { _projectTypes = [("haskell", [rootDir])] } root
    simpleTarget === project

test_resolve :: TestTree
test_resolve =
  testGroup "project resolution" [
    unitTest "canonicalize paths" test_canonicalPaths,
    unitTest "project type map" test_typeMap,
    unitTest "root marker" test_marker,
    unitTest "type base dirs" test_typeDirs,
    unitTest "explicitly typed projects" test_projectTypes
  ]
