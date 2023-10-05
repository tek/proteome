module Proteome.Tags.Mappings where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Exon (exon)
import Path (Abs, File, Path)
import Path.IO (doesFileExist)
import Ribosome (Rpc)
import Ribosome.Api (parseNvimFile)
import Ribosome.Host.Data.Report (ReportLog)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (MenuApp, MenuWidget, menuOk, menuState, withFocus', (%=))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.MenuState (mode)
import qualified Ribosome.Report as Report

import Proteome.Tags.State (Tag (Tag, line, path), TagsState, cycle)

data TagsAction =
  Navigate (Path Abs File) Int
  deriving stock (Eq, Show)

checkPath ::
  Members [Rpc, Embed IO] r =>
  Text ->
  Sem r (Maybe (Path Abs File))
checkPath path =
  runMaybeT do
    file <- MaybeT (parseNvimFile path)
    ifM (doesFileExist file) (pure file) empty

navigate ::
  Members [Rpc, ReportLog, Embed IO] r =>
  MenuWidget TagsState r TagsAction
navigate =
  withFocus' \ Tag {..} ->
    checkPath path >>= \case
      Just file ->
        pure (Just (Menu.success (Navigate file line)))
      Nothing ->
        Just MenuAction.Continue <$ Report.info [exon|File doesn't exist: #{path}|] [
          [exon|Tag file focused in menu doesn't exist: #{path}|]
          ]

cycleSegment :: MenuWidget TagsState r TagsAction
cycleSegment =
  menuState do
    mode . #segment %= cycle
    menuOk

mappings ::
  Members [Rpc, ReportLog, Embed IO] r =>
  MenuApp TagsState r TagsAction
mappings =
  [("<cr>", navigate), ("<c-s>", cycleSegment)]
