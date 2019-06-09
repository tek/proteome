{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module ResolveSpec (htf_thisModulesTests) where

import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import Path (absdir, parseAbsDir)
import Ribosome.File (canonicalPaths)
import Ribosome.Test.Unit (fixture)
import Test.Framework

import Project (fn, l, la, li, ta, ti, tp)
import Proteome.Config (defaultTypeMarkers)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectConfig (ProjectConfig(ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Data.ProjectType (ProjectType)
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
root = ProjectRoot [absdir|/projects/haskell/flagellum|]

typeMap :: Map ProjectType [ProjectType]
typeMap = Map.fromList [(tp, [ti, ta])]

config :: ProjectConfig
config =
  ProjectConfig [[absdir|/projects|]] def def typeMap def (Map.fromList [(tp, l)]) (Map.fromList [(l, [li, la])])

targetProject :: Project
targetProject =
  Project (DirProject fn root (Just tp)) [ti, ta] (Just l) [li, la]

test_typeMap :: IO ()
test_typeMap = do
  project <- resolveProject [] config (Just root) fn (Just tp)
  assertEqual targetProject project

markerConfig :: ProjectConfig
markerConfig = ProjectConfig def def def def defaultTypeMarkers def def

markerTarget :: ProjectRoot -> Project
markerTarget root' =
  Project (DirProject fn root' (Just tp)) [] (Just l) []

test_marker :: IO ()
test_marker = do
  dir <- parseAbsDir =<< fixture "projects/haskell/flagellum"
  let root' = ProjectRoot dir
  assertEqual (markerTarget root') =<< resolveProject [] markerConfig (Just root') fn Nothing
