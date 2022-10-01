module Proteome.Grep where

import qualified Data.Text as Text
import Exon (exon)
import Lens.Micro.Extras (view)
import Path (Abs, Dir, File, Path)
import Ribosome (
  Args,
  Handler,
  Report,
  Rpc,
  RpcError,
  Scratch,
  ScratchId (ScratchId),
  Settings,
  mapReport,
  pathText,
  pluginLogReports,
  resumeReport,
  toMsgpack,
  unArgs,
  )
import Ribosome.Api (nvimCallFunction, nvimCommand, nvimDir)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Register (setregLine)
import Ribosome.Api.Window (setCurrentCursor)
import qualified Ribosome.Data.Register as Register (Register (Special))
import Ribosome.Data.ScratchOptions (ScratchOptions (..))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  Filter (Fuzzy),
  Mappings,
  MenuItem,
  MenuWidget,
  WindowMenus,
  menuOk,
  menuState,
  modal,
  windowMenu,
  withFocus,
  withSelection,
  (%=),
  )
import Ribosome.Menu.Mappings (withInsert)
import Ribosome.Menu.MenuState (mode)
import qualified Ribosome.Settings as Settings
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (IsStream, SerialT)

import Proteome.Data.Env (Env)
import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError (EmptyUserInput))
import qualified Proteome.Data.GrepState as GrepState
import Proteome.Data.GrepState (GrepMode (GrepMode), GrepOutputLine (GrepOutputLine), GrepState, Segment (Full))
import Proteome.Data.ReplaceError (ReplaceError)
import Proteome.Grep.Process (defaultCmdline, grepCmdline, grepMenuItems)
import Proteome.Grep.Replace (deleteLines, replaceBuffer)
import Proteome.Grep.Syntax (grepSyntax)
import Proteome.Menu (handleResult)
import qualified Proteome.Settings as Settings (grepCmdline)

data GrepAction =
  Select (Path Abs File) Int (Maybe Int)
  |
  Replace (NonEmpty GrepOutputLine)
  |
  Delete (NonEmpty GrepOutputLine)
  |
  NoAction
  deriving stock (Eq, Show)

navigate ::
  Member Rpc r =>
  Path Abs File ->
  Int ->
  Maybe Int ->
  Sem r ()
navigate path line col = do
  edit path
  setCurrentCursor line (fromMaybe 0 col)
  nvimCommand "normal! zv"
  nvimCommand "normal! zz"

selectResult ::
  MenuWidget GrepState r GrepAction
selectResult = do
  withFocus \ (GrepOutputLine {file, line, col}) ->
    pure (Select file line col)

yankResult ::
  Members [Rpc, Resource, Embed IO] r =>
  MenuWidget GrepState r GrepAction
yankResult =
  withFocus \ (GrepOutputLine {content}) ->
    NoAction <$ setregLine (Register.Special "\"") [content]

replaceResult ::
  MenuWidget GrepState r GrepAction
replaceResult =
  menuState $ withSelection (pure . Replace)

deleteResult ::
  MenuWidget GrepState r GrepAction
deleteResult =
  menuState $ withSelection (pure . Delete)

menuItemSameLine :: MenuItem GrepOutputLine -> MenuItem GrepOutputLine -> Bool
menuItemSameLine l r =
  GrepState.sameLine (l ^. #meta) (r ^. #meta)

uniqBy ::
  Functor (t IO) =>
  IsStream t =>
  (a -> a -> Bool) ->
  t IO a ->
  t IO a
uniqBy f =
  Stream.catMaybes .
  Stream.smapM (flip check) (pure Nothing)
  where
    check new =
      pure . \case
        Just old | f old new ->
          (Just old, Nothing)
        Just _ ->
          (Just new, Just new)
        Nothing ->
          (Just new, Just new)

uniqueGrepLines ::
  Functor (t IO) =>
  IsStream t =>
  t IO (MenuItem GrepOutputLine) ->
  t IO (MenuItem GrepOutputLine)
uniqueGrepLines =
  uniqBy menuItemSameLine

grepItems ::
  Members [Settings !! SettingError, Rpc, Stop GrepError, Log, Embed IO, Final IO] r =>
  Path Abs Dir ->
  Text ->
  [Text] ->
  Sem r (SerialT IO (MenuItem GrepOutputLine))
grepItems path patt opt = do
  cwd <- nvimCwd
  userCmd <- Settings.maybe Settings.grepCmdline
  grepper <- maybe defaultCmdline pure userCmd
  (exe, args) <- grepCmdline grepper patt path opt
  items <- grepMenuItems cwd exe args
  pure (uniqueGrepLines items)

cycleSegment :: MenuWidget GrepState r GrepAction
cycleSegment =
  menuState do
    mode . #segment %= GrepState.cycle
    menuOk

actions ::
  Members [Scratch, Rpc, Rpc !! RpcError, AtomicState Env, Stop ReplaceError, Resource, Embed IO] r =>
  Mappings GrepState r GrepAction
actions =
  [
    ("<cr>", selectResult),
    ("y", yankResult),
    ("r", replaceResult),
    ("d", deleteResult),
    (withInsert "<c-s>", cycleSegment)
  ]

grepAction ::
  Members [Scratch, Rpc, Rpc !! RpcError, AtomicState Env, Stop ReplaceError, Resource, Embed IO] r =>
  GrepAction ->
  Sem r ()
grepAction = \case
  Select path line col ->
    navigate path line col
  Replace results ->
    replaceBuffer results
  Delete results ->
     deleteLines (toList results)
  NoAction ->
    unit

type GrepErrorStack =
  [Scratch, Settings, Rpc, Stop ReplaceError, Stop GrepError]

handleErrors ::
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, Stop Report] r =>
  InterpretersFor GrepErrorStack r
handleErrors =
  mapReport @GrepError .
  mapReport @ReplaceError .
  pluginLogReports

type GrepStack =
  [
    WindowMenus () GrepState !! RpcError,
    Settings !! SettingError,
    Scratch !! RpcError,
    Rpc !! RpcError,
    AtomicState Env,
    Log,
    Resource,
    Embed IO,
    Final IO
  ]

grepWith ::
  Members GrepStack r =>
  Members GrepErrorStack r =>
  Member (Stop Report) r =>
  [Text] ->
  Path Abs Dir ->
  Text ->
  Sem r ()
grepWith opt path patt =
  mapReport @RpcError do
    items <- grepItems path patt opt
    result <- windowMenu items (modal (GrepMode Fuzzy Full)) (def & #items .~ scratchOptions) actions
    handleResult grepAction result
  where
    scratchOptions =
      def {
        name = ScratchId name,
        syntax = [grepSyntax],
        filetype = Just name
      }
    name =
      "proteome-grep"

askUser ::
  Members [Rpc, Stop GrepError] r =>
  Text ->
  [Text] ->
  Sem r Text
askUser purpose args = do
  spec <- nvimCallFunction "input" (toMsgpack <$> [exon|#{purpose}: |] : args)
  if Text.null spec then stop (GrepError.EmptyUserInput purpose) else pure spec

grepWithNative ::
  Members GrepStack r =>
  [Text] ->
  Maybe Text ->
  Maybe Args ->
  Handler r ()
grepWithNative opt pathSpec pattSpec = do
  handleErrors do
    path <- nvimDir =<< maybe (askUser "directory" [".", "dir"]) pure pathSpec
    patt <- resumeReport @Rpc $ mapReport @GrepError do
      maybe (askUser "pattern" []) (pure . unArgs) pattSpec
    grepWith opt path patt

proGrepIn ::
  Members GrepStack r =>
  Maybe Text ->
  Maybe Args ->
  Handler r ()
proGrepIn =
  grepWithNative []

proGrepOpt ::
  Members GrepStack r =>
  Text ->
  Maybe Args ->
  Handler r ()
proGrepOpt opt patt = do
  cwd <- resumeReport @Rpc $ mapReport @GrepError do
    nvimCwd
  grepWithNative (Text.words opt) (Just (pathText cwd)) patt

proGrepOptIn ::
  Members GrepStack r =>
  Text ->
  Maybe Text ->
  Maybe Args ->
  Handler r ()
proGrepOptIn opt =
  grepWithNative (Text.words opt)

proGrep ::
  Members GrepStack r =>
  Maybe Args ->
  Handler r ()
proGrep =
  proGrepOpt ""

proGrepList ::
  Members GrepStack r =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Handler r [GrepOutputLine]
proGrepList patt pathSpec opt = do
  path <- resumeReport @Rpc (nvimDir (fromMaybe "." pathSpec))
  items <- handleErrors (grepItems path patt (Text.words (fold opt)))
  fmap (view #meta) <$> embed (Stream.toList items)
