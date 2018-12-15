module Config(
  vars,
) where

import Neovim (toObject)
import System.Directory (getCurrentDirectory)
import Ribosome.Test.Embed (Vars(..))

vars :: IO Vars
vars = do
  base <- getCurrentDirectory
  return $ Vars [
    ("proteome_project_base_dirs", toObject [base ++ "/test/f/fixtures/projects"]),
    ("proteome_main_project_dir", toObject $ base ++ "/test/f/fixtures/projects/haskell/flagellum")
    ]
