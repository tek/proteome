module Proteome.Test.AddMenuTest where

import Hedgehog ((===))
import Path (parseAbsDir, parseRelDir, (</>))
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent
import Ribosome.Menu.Prompt.Run (noPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (serial)

import Proteome.Add (addMenuWith)
import qualified Proteome.Data.Env as Env
import Proteome.Data.Env (Env)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import qualified Proteome.Settings as Settings
import Proteome.Test.Project (flag, fn, hask, l, tp)
import Proteome.Test.Unit (ProteomeTest, testDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInput chars' =
  PromptInput \ _ ->
    serial (Streamly.nilM (sleep 0.1)) (Streamly.fromList (PromptInputEvent.Character <$> chars'))

promptConfig ::
  MonadIO m =>
  [Text] ->
  PromptConfig m
promptConfig cs =
  PromptConfig (promptInput cs) basicTransition noPromptRenderer []

addChars :: [Text]
addChars =
  ["k", "cr"]

addMenuSpec :: ProteomeTest ()
addMenuSpec = do
  projectsDir <- parseAbsDir =<< fixture "projects"
  updateSetting Settings.projectConfig (ProjectConfig [projectsDir] def def def def def def)
  addMenuWith (promptConfig addChars)
  projects <- getL @Env Env.projects
  haskPath <- parseRelDir (toString hask)
  flagPath <- parseRelDir (toString flag)
  let root = projectsDir </> haskPath </> flagPath
  [Project (DirProject fn (ProjectRoot root) (Just tp)) [] (Just l) []] === projects

test_addMenu :: UnitTest
test_addMenu =
  testDef addMenuSpec
