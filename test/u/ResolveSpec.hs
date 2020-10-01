{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module ResolveSpec (htf_thisModulesTests) where

import qualified Data.Map as Map (fromList)
import Path (absdir, parseAbsDir)
import Ribosome.File (canonicalPaths)
import Test.Framework

import Project (fn, l, la, li, ta, ti, tp)
import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Env (Proteome)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType)
import Proteome.Project.Resolve (resolveProject)
import Unit (specDef)

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
root = ProjectRoot [absdir|/projects/haskell/flagellum|]

typeMap :: Map ProjectType [ProjectType]
typeMap = Map.fromList [(tp, [ti, ta])]

config :: ProjectConfig
config =
  ProjectConfig [[absdir|/projects|]] def def typeMap def (Map.fromList [(tp, l)]) (Map.fromList [(l, [li, la])])

targetProject :: Project
targetProject =
  Project (DirProject fn root (Just tp)) [ti, ta] (Just l) [li, la]

typeMapSpec :: Proteome ()
typeMapSpec = do
  project <- resolveProject [] config (Just root) fn (Just tp)
  gassertEqual targetProject project

test_typeMap :: IO ()
test_typeMap =
  specDef typeMapSpec

markerConfig :: ProjectConfig
markerConfig = ProjectConfig def def def def defaultTypeMarkers def def

markerTarget :: ProjectRoot -> Project
markerTarget root' =
  Project (DirProject fn root' (Just tp)) [] (Just l) []

markerSpec :: Proteome ()
markerSpec = do
  dir <- parseAbsDir =<< fixture "projects/haskell/flagellum"
  let root' = ProjectRoot dir
  project <- resolveProject [] markerConfig (Just root') fn Nothing
  gassertEqual (markerTarget root') project

test_marker :: IO ()
test_marker =
  specDef markerSpec
