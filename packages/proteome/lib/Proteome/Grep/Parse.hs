module Proteome.Grep.Parse where

import Chiasma.Data.Ident (generateIdent, identText)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as Text (strip)
import Exon (exon)
import qualified Log
import Path (Abs, Dir, Path, parseAbsFile, parseRelFile, stripProperPrefix, (</>))
import Ribosome (pathText)
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Text.Parser.Char (anyChar, char, noneOf)
import Text.Parser.Combinators (manyTill)
import Text.Parser.Token (TokenParsing, natural)

import Proteome.Data.GrepState (GrepOutputLine (GrepOutputLine), grepOutputLine)
import Proteome.Grep.Syntax (lineNumber)

grepParser ::
  MonadFail m =>
  TokenParsing m =>
  Path Abs Dir ->
  m GrepOutputLine
grepParser cwd =
  grepOutputLine <$> path <*> (subtract 1 <$> number) <*> optional number <*> (toText <$> many anyChar)
  where
    path = do
      s <- manyTill (noneOf ":") (char ':')
      maybe (fail "not a path") pure (parseAbsFile s <|> ((cwd </>) <$> parseRelFile s))
    number =
      (fromInteger <$> natural) <* char ':'

formatGrepLine :: Path Abs Dir -> GrepOutputLine -> Text
formatGrepLine cwd (GrepOutputLine file line col content _ _ _) =
  [exon|#{relativePath} #{lineNumber} #{show line}:#{show (fromMaybe 1 col)} #{Text.strip content}|]
  where
    relativePath =
      maybe (pathText file) pathText (stripProperPrefix cwd file)

parseGrepOutput ::
  Members [Log, Embed IO] r =>
  Path Abs Dir ->
  Text ->
  Sem r (Maybe (MenuItem GrepOutputLine))
parseGrepOutput cwd =
  item . parseOnly (grepParser cwd)
  where
    item (Right a) = do
      ident <- identText <$> generateIdent
      pure (Just (convert ident a))
    item (Left err) =
      Nothing <$ Log.debug [exon|parsing grep output failed: #{toText err}|]
    convert _ file =
      MenuItem file text [exon| * #{text}|]
      where
        text =
          formatGrepLine cwd file
