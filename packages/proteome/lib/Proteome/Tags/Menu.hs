module Proteome.Tags.Menu where

import qualified Data.Text as Text
import Exon (exon)
import Path.IO (doesFileExist)
import Prelude hiding (tag)
import Ribosome (
  Args (Args),
  Handler,
  Report,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  SettingError,
  Settings,
  mapReport,
  pathText,
  resumeReport,
  )
import Ribosome.Host.Data.Report (ReportLog)
import Ribosome.Menu (Filter (Fuzzy), MenuItem, MenuResult, WindowMenus, modal, windowMenu)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Proteome.Data.Env (Env, mainType)
import Proteome.Menu (handleResult)
import Proteome.Tags.Cycle (cword)
import Proteome.Tags.Mappings (TagsAction (Navigate), mappings)
import Proteome.Tags.Nav (loadOrEdit)
import Proteome.Tags.Query (query)
import Proteome.Tags.State (
  RawTagSegments,
  Segment (Module, Name),
  Tag,
  TagSegments,
  TagsMode (TagsMode),
  TagsState,
  tagSegmentsFor,
  )
import Proteome.Tags.Stream (readTags)
import Proteome.Tags.Syntax (tagsSyntax)

getTags ::
  Members [AtomicState Env, Rpc] r =>
  (RawTagSegments -> TagSegments) ->
  Maybe Text ->
  Sem r (SerialT IO (MenuItem Tag))
getTags mkSegments = \case
  Just rex -> do
    Stream.fromList <$> query mkSegments rex
  Nothing ->
    readTags mkSegments

tagsAction ::
  Members [Rpc, Stop Report, Embed IO] r =>
  TagsAction ->
  Sem r ()
tagsAction = \case
  Navigate path line -> do
    unlessM (doesFileExist path) do
      stop (fromText [exon|File doesn't exist: #{pathText path}|])
    loadOrEdit path line

type TagsStack =
  [
    WindowMenus () TagsState !! RpcError,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log
  ]

tagsMenu ::
  Members TagsStack r =>
  Members [AtomicState Env, Rpc, ReportLog, Stop Report, Embed IO] r =>
  Maybe Text ->
  Sem r (MenuResult TagsAction)
tagsMenu rex = do
  tpe <- atomicGets mainType
  tags <- getTags (tagSegmentsFor tpe) rex
  mapReport do
    windowMenu tags (modal (TagsMode Fuzzy mode)) (def & #items .~ scratchOptions) mappings
  where
    mode =
      if isJust rex then Module else Name
    scratchOptions =
      def
      & #name .~ ScratchId name
      & #syntax .~ [tagsSyntax]
      & #filetype ?~ name
    name =
      "proteome-tags"

tagsMenuHandle ::
  Members TagsStack r =>
  Members [AtomicState Env, Rpc, ReportLog, Stop Report, Embed IO] r =>
  Maybe Text ->
  Sem r ()
tagsMenuHandle =
  handleResult tagsAction <=< tagsMenu

proTags ::
  Members TagsStack r =>
  Members [AtomicState Env, ReportLog, Embed IO] r =>
  Args ->
  Handler r ()
proTags (Args rex) =
  resumeReport @Rpc do
    tagsMenuHandle (if Text.null rex then Nothing else Just rex)

exactQuery ::
  Member Rpc r =>
  Text ->
  Sem r Text
exactQuery =
  fmap exact . \case
    "" ->
      cword
    rex ->
      pure rex
  where
    exact rex =
      [exon|^#{rex}$|]

proTag ::
  Members TagsStack r =>
  Members [AtomicState Env, ReportLog, Embed IO] r =>
  Args ->
  Handler r ()
proTag (Args name) = do
  resumeReport @Rpc do
    rex <- exactQuery name
    tagsMenuHandle (Just rex)
