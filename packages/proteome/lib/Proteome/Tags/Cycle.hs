module Proteome.Tags.Cycle where

import qualified Data.List.NonEmpty.Zipper as Zipper
import Exon (exon)
import Log (Severity (Info))
import Path (Abs, File, Path)
import Ribosome (Args (Args), Handler, Report (Report), Rpc, RpcError, resumeReport, toMsgpack)
import Ribosome.Api (bufferGetOption, currentBufferPath, currentCursor, nvimCallFunction, nvimGetCurrentBuf, wipeBuffer)

import qualified Proteome.Data.CurrentTag as CurrentTag
import Proteome.Data.CurrentTag (pattern CurrentLoc, CurrentTag (CurrentTag), cycleLoc)
import Proteome.Data.Env (Env)
import Proteome.Tags.Nav (loadOrEdit)
import Proteome.Tags.Query (tagLocsPath)
import qualified Proteome.Tags.State as State
import Proteome.Tags.State (TagLoc (TagLoc))

nav ::
  Members [AtomicState (Maybe CurrentTag), Rpc] r =>
  CurrentTag ->
  Sem r ()
nav cur@(CurrentLoc TagLoc {..}) = do
  buf <- nvimGetCurrentBuf
  loaded <- loadOrEdit path line
  newBuf <- nvimGetCurrentBuf
  unless (cur ^. #bufferWasLoaded || buf == newBuf) do
    unlessM (bufferGetOption buf "modified") do
      wipeBuffer buf
  atomicPut (Just (cur & #bufferWasLoaded .~ loaded))

continue ::
  Member Rpc r =>
  CurrentTag ->
  Sem r Bool
continue CurrentTag {locations = Zipper.current -> TagLoc {..}} = do
  (bufLine, bufCol) <- currentCursor
  bufPath <- currentBufferPath
  pure (elem path bufPath && bufLine == line && bufCol == col)

cycle ::
  Members [AtomicState (Maybe CurrentTag), Rpc] r =>
  CurrentTag ->
  Sem r ()
cycle =
  nav . (#locations %~ cycleLoc)

storeAndNav ::
  Members [AtomicState (Maybe CurrentTag), Rpc] r =>
  Text ->
  NonEmpty (TagLoc (Path Abs File)) ->
  Sem r ()
storeAndNav name locs = do
  nav (CurrentTag name (Zipper.fromNonEmpty locs) True)

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
      ifM (continue cur) (cycle cur) (start name)
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
