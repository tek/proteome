module Proteome.Tags.Cycle where

import qualified Data.List.NonEmpty.Zipper as Zipper
import Exon (exon)
import Log (Severity (Info))
import Path (Abs, File, Path)
import Ribosome (Args (Args), Report (Report), Rpc, toMsgpack, Handler, RpcError, resumeReport)
import Ribosome.Api (currentBufferPath, currentLine, nvimCallFunction)

import qualified Proteome.Data.CurrentTag as CurrentTag
import Proteome.Data.CurrentTag (pattern CurrentLoc, CurrentTag (CurrentTag), cycleLoc)
import Proteome.Data.Env (Env)
import Proteome.Tags.Nav (loadOrEdit)
import Proteome.Tags.Query (tagLocsPath)
import qualified Proteome.Tags.State as State
import Proteome.Tags.State (TagLoc (TagLoc))

nav ::
  Member Rpc r =>
  CurrentTag ->
  Sem r ()
nav (CurrentLoc TagLoc {..}) =
  loadOrEdit path line

continue ::
  Member Rpc r =>
  CurrentTag ->
  Text ->
  Sem r Bool
continue CurrentTag {locations = Zipper.current -> TagLoc {..}} newName = do
  bufLine <- currentLine
  bufPath <- currentBufferPath
  pure (elem path bufPath && bufLine == line && name == newName)

cycle ::
  Members [AtomicState (Maybe CurrentTag), Rpc] r =>
  CurrentTag ->
  Sem r ()
cycle cur = do
  atomicPut (Just newCur)
  nav newCur
  where
    newCur =
      cur & #locations %~ cycleLoc

storeAndNav ::
  Members [AtomicState (Maybe CurrentTag), Rpc] r =>
  Text ->
  NonEmpty (TagLoc (Path Abs File)) ->
  Sem r ()
storeAndNav name locs = do
  atomicPut (Just cur)
  nav cur
  where
    cur =
      CurrentTag name (Zipper.fromNonEmpty locs)

start ::
  Members [AtomicState (Maybe CurrentTag), AtomicState Env, Rpc, Stop Report, Embed IO] r =>
  Text ->
  Sem r ()
start name = do
  file <- currentBufferPath
  tags <- nonEmpty <$> tagLocsPath (Just [exon|^#{name}$|]) file
  maybe (stop (Report err [err] Info)) (storeAndNav name) tags
  where
    err =
      [exon|No matching tag for #{name}|]

nextTag ::
  Members [AtomicState (Maybe CurrentTag), AtomicState Env, Rpc, Stop Report, Embed IO] r =>
  Text ->
  Sem r ()
nextTag name =
  atomicGet >>= \case
    Just cur ->
      ifM (continue cur name) (cycle cur) (start name)
    Nothing ->
      start name

cword ::
  Member Rpc r =>
  Sem r Text
cword =
  nvimCallFunction "expand" [toMsgpack @Text "<cword>"]

proNextTag ::
  Members [AtomicState (Maybe CurrentTag), AtomicState Env, Rpc !! RpcError, Embed IO] r =>
  Args ->
  Handler r ()
proNextTag =
  resumeReport . \case
    Args "" ->
      nextTag =<< cword
    Args name ->
      nextTag name
