{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TypeMapSpec(
  htf_thisModulesTests
) where

import Control.Lens (set)
import Test.Framework
import qualified Ribosome.Data.Ribo as Ribo (modify)
import Ribosome.Test.Unit (tempDir)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Env (_mainProject)
import Proteome.Data.Project (
  ProjectName(..),
  ProjectRoot(..),
  ProjectType(..),
  ProjectLang(..),
  ProjectMetadata(DirProject),
  _meta,
  _lang,
  )
import Proteome.Test.Unit (specWithDef)
import Config (vars)

main :: FilePath -> ProjectMetadata
main root = DirProject (ProjectName "flagellum") (ProjectRoot root) (Just (ProjectType "haskell"))

typeMapSpec :: Proteome ()
typeMapSpec = do
  root <- tempDir "projects/haskell/flagellum"
  Ribo.modify $ set (_mainProject._meta) (main root)
  Ribo.modify $ set (_mainProject._lang) (Just (ProjectLang "idris"))

test_typeMap :: IO ()
test_typeMap = vars >>= specWithDef typeMapSpec
