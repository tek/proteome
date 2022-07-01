module Proteome.Diag where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Path (toFilePath)
import Prettyprinter (Doc, line, nest, pretty, vsep)
import Ribosome (
  ErrorMessage (ErrorMessage),
  Errors,
  Handler,
  HandlerTag (GlobalTag, HandlerTag),
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  Settings,
  StoredError (StoredError),
  resumeHandlerError,
  scratch,
  )
import qualified Ribosome.Errors as Errors
import qualified Ribosome.Scratch as Scratch
import Ribosome.Scratch (ScratchOptions (filetype, focus))

import qualified Proteome.Data.Env as Env
import Proteome.Data.Env (Env)
import Proteome.Data.Project (Project (Project))
import Proteome.Data.ProjectLang (ProjectLang (ProjectLang, unProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata (DirProject, VirtualProject))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectRoot (ProjectRoot (ProjectRoot))
import Proteome.Data.ProjectType (ProjectType (ProjectType, unProjectType))
import Proteome.Tags (tagsCommand)

formatLang :: Maybe ProjectLang -> Text
formatLang (Just (ProjectLang lang)) = lang
formatLang Nothing = "none"

formatType :: Maybe ProjectType -> Text
formatType (Just (ProjectType tpe)) = tpe
formatType Nothing = "none"

formatProject :: ProjectName -> ProjectRoot -> Maybe ProjectType -> [Text]
formatProject (ProjectName name) (ProjectRoot root) tpe =
  [
    "name: " <> name,
    "root: " <> (toText . toFilePath) root,
    "type: " <> formatType tpe
  ]

formatMeta ::
  Member (Settings !! se) r =>
  ProjectMetadata ->
  [ProjectLang] ->
  Sem r [Text]
formatMeta (VirtualProject (ProjectName name)) _ =
  pure ["name: " <> name]
formatMeta (DirProject name root tpe) langs = do
  tags :: [Text] <- resumeAs [] do
    runStop (tagsCommand root langs) >>= \case
      Right (tagsCmd, tagsArgs) ->
        pure [[exon|tags cmd: #{tagsCmd} #{tagsArgs}|]]
      Left _ ->
        pure []
  pure (formatProject name root tpe <> tags)

formatMain ::
  Member (Settings !! se) r =>
  Project ->
  Sem r [Text]
formatMain (Project meta types lang langs) = do
  metaContent <- formatMeta meta langs
  pure $ metaContent <> [
    "types: " <> Text.intercalate ", " (unProjectType <$> types),
    "main language: " <> formatLang lang,
    "languages: " <> Text.intercalate ", " (unProjectLang <$> langs)
    ]

formatExtraProjects ::
  Member (Settings !! se) r =>
  [Project] ->
  Sem r [Text]
formatExtraProjects projects = do
  formatted <- traverse formatMain projects
  pure $ ["", "Extra projects", ""] <> intercalate [""] formatted

formatExtraProjectsIfNonempty ::
  Members [Settings !! se, AtomicState Env] r =>
  Sem r [Text]
formatExtraProjectsIfNonempty = do
  projects <- atomicGets Env.projects
  case projects of
    _ : _ -> formatExtraProjects projects
    _ -> pure []

tagName :: HandlerTag -> Text
tagName = \case
  GlobalTag -> "global"
  HandlerTag n -> n

storedError :: StoredError -> Doc a
storedError (StoredError (ErrorMessage _ log _) _) =
  case log of
    [] -> mempty
    (h : t) ->
      nest 2 (vsep (pretty <$> ([exon|* #{h}|] : t)))

tagErrors :: HandlerTag -> [StoredError] -> Doc a
tagErrors t errs =
  pretty [exon|### #{tagName t}|] <> line <> vsep (storedError <$> errs)

errorDiagnostics :: Map HandlerTag [StoredError] -> Doc a
errorDiagnostics errs | null errs =
  mempty
errorDiagnostics errs =
  "## Errors" <> line <> line <> vsep (uncurry tagErrors <$> Map.toAscList errs)

diagnostics ::
  Members [Settings !! se, AtomicState Env, Errors] r =>
  Sem r [Text]
diagnostics = do
  main <- formatMain =<< atomicGets Env.mainProject
  extra <- formatExtraProjectsIfNonempty
  confLog <- atomicGets Env.configLog
  errors <- errorDiagnostics <$> Errors.get
  pure $ header <> main <> extra <> ["", "loaded config files:"] <> confLog <> Text.lines (show errors)
  where
    header =
      ["Diagnostics", "", "Main project", ""]

proDiag ::
  Members [Settings !! se, Scratch !! RpcError, AtomicState Env, Errors] r =>
  Handler r ()
proDiag = do
  resumeHandlerError @Scratch do
    content <- diagnostics
    void $ Scratch.show content options
  where
    options =
      (scratch (ScratchId name)) {
        focus = True,
        filetype = Just name
      }
    name =
      "proteome-diagnostics"
