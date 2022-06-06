module Proteome.Test.ResolveTest where

import qualified Data.Map.Strict as Map
import Path (Abs, Dir, Path, absdir, parent, reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, unitTest, (===))
import Ribosome.Test (testError)
import Test.Tasty (TestTree, testGroup)

import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project (Project))
import qualified Proteome.Data.ProjectConfig as ProjectConfig
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType)
import Proteome.Project.Resolve (fromName, fromRoot)
import Proteome.Test.Project (fn, l, la, li, ta, ti, tp)
import Proteome.Test.Run (proteomeTest)

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
  proteomeTest do
    dir <- Test.fixturePath [reldir|projects/haskell/flagellum|]
    project <- testError (fromName [] (config (parent (parent dir))) fn (Just tp))
    targetProject dir === project

markerTarget :: ProjectRoot -> Project
markerTarget root' =
  Project (DirProject fn root' (Just tp)) [] (Just l) []

test_marker :: UnitTest
test_marker =
  proteomeTest do
    (ProjectRoot -> dir) <- Test.fixturePath [reldir|projects/haskell/flagellum|]
    project <- testError (fromRoot [] def { ProjectConfig.typeMarkers = defaultTypeMarkers } dir)
    markerTarget dir === project

test_typeDirs :: UnitTest
test_typeDirs =
  proteomeTest do
    project <- testError (fromRoot [] def { ProjectConfig.typeDirs = [("haskell", [parent rootDir])] } root)
    simpleTarget === project

test_projectTypes :: UnitTest
test_projectTypes =
  proteomeTest do
    project <- testError (fromRoot [] def { ProjectConfig.projectTypes = [("haskell", [rootDir])] } root)
    simpleTarget === project

test_resolve :: TestTree
test_resolve =
  testGroup "project resolution" [
    unitTest "project type map" test_typeMap,
    unitTest "root marker" test_marker,
    unitTest "type base dirs" test_typeDirs,
    unitTest "explicitly typed projects" test_projectTypes
  ]
