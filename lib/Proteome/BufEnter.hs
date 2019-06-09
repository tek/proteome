module Proteome.BufEnter where

import qualified Data.Text as Text (intercalate)
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (bufferSetOption, vimGetCurrentBuffer)

import Proteome.Data.Env (Env)
import Proteome.Data.Project (Project(Project))
import Proteome.Data.ProjectMetadata (ProjectMetadata(DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot(ProjectRoot))
import Proteome.Project (allProjects)
import Proteome.Settings (tagsFileName)

setBufferTags ::
  NvimE e m =>
  [Path Abs File] ->
  m ()
setBufferTags tags = do
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "tags" (toMsgpack $ Text.intercalate "," (toText . toFilePath <$> tags))

projectRoot :: Project -> Maybe (Path Abs Dir)
projectRoot (Project (DirProject _ (ProjectRoot root) _) _ _ _) = Just root
projectRoot _ = Nothing

bufEnter ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  m ()
bufEnter = do
  roots <- mapMaybe projectRoot <$> allProjects
  name <- setting tagsFileName
  let tags = fmap (</> name) roots
  setBufferTags tags
