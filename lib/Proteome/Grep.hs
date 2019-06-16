module Proteome.Grep where

import Chiasma.Data.Ident (generateIdent, identText)
import Conduit (ConduitT, (.|))
import Control.Monad.Catch (MonadThrow)
import Data.Attoparsec.Text (parseOnly)
import Data.Composition ((.:))
import qualified Data.Conduit.Combinators as Conduit (decodeUtf8, linesUnbounded)
import qualified Data.Conduit.List as Conduit (mapMaybeM)
import Data.Conduit.Process.Typed (createSource)
import qualified Data.Map as Map (fromList)
import Data.Text (isInfixOf)
import qualified Data.Text as Text (breakOn, null, replace, splitOn, strip, stripPrefix, take)
import Path (Abs, File, Path, parseAbsFile, parseRelFile, toFilePath)
import Path.IO (findExecutable, isLocationOccupied)
import Ribosome.Api.Buffer (edit)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Api.Window (setCurrentCursor)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, menuContinue, menuQuitWith, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCommand)
import System.Process.Typed (
  Process,
  getStdout,
  proc,
  setStdout,
  startProcess,
  )
import Text.Parser.Char (anyChar, char, newline, noneOf)
import Text.Parser.Combinators (manyTill, try)
import Text.Parser.Token (TokenParsing, natural)

import Proteome.Data.GrepError (GrepError)
import qualified Proteome.Data.GrepError as GrepError (GrepError(..))
import Proteome.Grep.Syntax (grepSyntax, lineNumber)
import qualified Proteome.Settings as Settings (grepCmdline)

patternPlaceholder :: Text
patternPlaceholder =
  "${pattern}"

pathPlaceholder :: Text
pathPlaceholder =
  "${path}"

replaceOrAppend :: Text -> Text -> [Text] -> [Text]
replaceOrAppend placeholder target segments | any (placeholder `isInfixOf`) segments =
  Text.replace placeholder target <$> segments
replaceOrAppend _ target segments =
  segments <> [target]

parseAbsExe ::
  MonadDeepError e GrepError m =>
  Text ->
  m (Path Abs File)
parseAbsExe exe =
  hoistEitherAs (GrepError.NoSuchExecutable exe) $ parseAbsFile (toString exe)

findExe ::
  MonadIO m =>
  MonadDeepError e GrepError m =>
  Text ->
  m (Path Abs File)
findExe exe = do
  path <- hoistEitherAs parseError $ parseRelFile (toString exe)
  hoistMaybe notInPath =<< findExecutable path
  where
    parseError =
      GrepError.NoSuchExecutable exe
    notInPath =
      GrepError.NotInPath exe

grepCmdline ::
  Text ->
  Text ->
  Text ->
  Text ->
  MonadIO m =>
  MonadDeepError e GrepError m =>
  m (Text, [Text])
grepCmdline cmdline patt cwd destination = do
  when (Text.null exe) $ throwHoist GrepError.Empty
  absExe <- if absolute exe then parseAbsExe exe else findExe exe
  destPath <- hoistEitherAs destError $ parseAbsFile (toString (absDestination destination))
  unlessM (isLocationOccupied destPath) $ throwHoist destError
  return (toText (toFilePath absExe), withDir destPath)
  where
    absDestination d =
      if absolute d then d else cwd <> "/" <> d
    absolute a =
      Text.take 1 a == "/"
    argSegments =
      Text.splitOn " " (Text.strip args)
    (exe, args) =
      Text.breakOn " " (Text.strip cmdline)
    withDir dir =
      replaceOrAppend pathPlaceholder (toText (toFilePath dir)) withPattern
    withPattern =
      replaceOrAppend patternPlaceholder patt argSegments
    destError =
      GrepError.NoSuchDestination destination

grepProcess ::
  NvimE e m =>
  MonadIO m =>
  Text ->
  [Text] ->
  m (Process () (ConduitT () ByteString m ()) ())
grepProcess exe args =
  startProcess (config (toString exe) (toString <$> args))
  where
    config =
      setStdout createSource .: proc

data GrepOutputLine =
  GrepOutputLine Text Int (Maybe Int) Text
  deriving (Eq, Show)

grepParser ::
  TokenParsing m =>
  m GrepOutputLine
grepParser =
  GrepOutputLine <$> path <*> (subtract 1 <$> number) <*> optional number <*> (toText <$> many anyChar)
  where
    path =
      toText <$> manyTill (noneOf ":") (char ':')
    number =
      (fromInteger <$> natural) <* char ':'

formatGrepLine :: Text -> GrepOutputLine -> Text
formatGrepLine cwd (GrepOutputLine path line col text) =
  relativePath <> " " <> lineNumber <> " " <> show line <> formatCol col <> text
  where
    formatCol (Just c) =
      "/" <> show c
    formatCol Nothing =
      ""
    relativePath =
      fromMaybe path (Text.stripPrefix (cwd <> "/") path)

parseGrepOutput ::
  MonadRibo m =>
  Text ->
  Text ->
  m (Maybe (MenuItem GrepOutputLine))
parseGrepOutput cwd =
  item . parseOnly grepParser
  where
    item (Right a) = do
      ident <- identText <$> generateIdent
      return (Just (convert ident a))
    item (Left err) =
      Nothing <$ logDebug ("parsing grep output failed: " <> err)
    convert _ grepLine =
      MenuItem grepLine (formatGrepLine cwd grepLine)

grep ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  Text ->
  Text ->
  [Text] ->
  ConduitT () (MenuItem GrepOutputLine) m ()
grep cwd exe args = do
  prc <- lift $ grepProcess exe args
  getStdout prc .| Conduit.decodeUtf8 .| Conduit.linesUnbounded .| Conduit.mapMaybeM (parseGrepOutput cwd)

navigate ::
  NvimE e m =>
  Text ->
  Int ->
  Maybe Int ->
  m ()
navigate path line col = do
  edit (toString path)
  setCurrentCursor line (fromMaybe 0 col)
  vimCommand "normal! zv"
  vimCommand "normal! zz"

selectResult ::
  NvimE e m =>
  MonadRibo m =>
  Menu GrepOutputLine ->
  Prompt ->
  m (MenuConsumerAction m (), Menu GrepOutputLine)
selectResult menu _ =
  check $ selectedMenuItem menu
  where
    check (Just (MenuItem (GrepOutputLine path line col _) _)) =
      menuQuitWith (navigate path line col) menu
    check Nothing =
      menuContinue menu

proGrepWith ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  PromptConfig m ->
  Text ->
  Text ->
  m ()
proGrepWith promptConfig path patt = do
  grepper <- setting Settings.grepCmdline
  cwd <- toText <$> nvimCwd
  (exe, args) <- grepCmdline grepper patt cwd path
  void $ nvimMenu scratchOptions (grep cwd exe args) handler promptConfig
  where
    scratchOptions =
      scratchSyntax [grepSyntax] . defaultScratchOptions $ "proteome-grep"
    handler =
      defaultMenu (Map.fromList [("cr", selectResult)])

proGrepIn ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Text ->
  Text ->
  m ()
proGrepIn =
  proGrepWith promptConfig
  where
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer False

proGrep ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepError e GrepError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Text ->
  m ()
proGrep patt = do
  cwd <- nvimCwd
  proGrepIn (toText cwd) patt
