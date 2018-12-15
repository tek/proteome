module Proteome.BufEnter(
  bufEnter,
) where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import System.FilePath ((</>))
import Neovim (buffer_set_option', Neovim, vim_get_current_buffer', toObject)
import Ribosome.Config.Setting (setting)
import Proteome.Data.Proteome (Proteome)
import Proteome.Data.Project (Project(Project), ProjectMetadata(DirProject), ProjectRoot(ProjectRoot))
import Proteome.Project (allProjects)
import Proteome.Settings (tagsFileName)

setBufferTags :: [String] -> Neovim e ()
setBufferTags tags = do
  buf <- vim_get_current_buffer'
  buffer_set_option' buf "tags" (toObject $ intercalate "," tags)

projectRoot :: Project -> Maybe FilePath
projectRoot (Project (DirProject _ (ProjectRoot root) _) _ _ _) = Just root
projectRoot _ = Nothing

bufEnter :: Proteome ()
bufEnter = do
  pros <- allProjects
  name <- setting tagsFileName
  let roots = pros >>= maybeToList . projectRoot
  let tags = fmap (</> name) roots
  setBufferTags tags
