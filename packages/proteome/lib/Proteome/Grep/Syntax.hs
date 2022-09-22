module Proteome.Grep.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.Syntax.Syntax (HiLink (HiLink), Syntax (Syntax))
import Ribosome.Data.SyntaxItem (SyntaxItem (options, params))
import Ribosome.Syntax (syntaxMatch, syntaxVerbatim)
import Text.RawString.QQ (r)

lineNumber :: Text
lineNumber =
  "\57505"

asterisk :: SyntaxItem
asterisk =
  item {options, params}
  where
    item = syntaxMatch "ProGrepAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepPath")]

path :: SyntaxItem
path =
  item {options, params}
  where
    item = syntaxMatch "ProGrepPath" ([r|.*\ze|] <> lineNumber)
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepLN")]

ln :: SyntaxItem
ln =
  item {options, params}
  where
    item = syntaxMatch "ProGrepLN" lineNumber
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepLine")]

line :: SyntaxItem
line =
  item {options, params}
  where
    item = syntaxMatch "ProGrepLine" [r|\d\+\ze:|]
    options = ["contained"]
    params = Map.fromList [("nextgroup", "ProGrepColon")]

colon :: SyntaxItem
colon =
  item {options, params}
  where
    item = syntaxMatch "ProGrepColon" [r|:|]
    options = ["contained"]
    params = Map.fromList [("nextgroup", "ProGrepCol")]

col :: SyntaxItem
col =
  item {options, params}
  where
    item = syntaxMatch "ProGrepCol" [r|\d\+|]
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepText")]

text_ :: SyntaxItem
text_ =
  item { options }
  where
    item = syntaxMatch "ProGrepText" [r|.\+|]
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProGrepAsterisk" "Todo"

hlPath :: HiLink
hlPath =
  HiLink "ProGrepPath" "Type"

hlLn :: HiLink
hlLn =
  HiLink "ProGrepLN" "LineNr"

hlLine :: HiLink
hlLine =
  HiLink "ProGrepLine" "Constant"

hlCol :: HiLink
hlCol =
  HiLink "ProGrepCol" "Constant"

hlText :: HiLink
hlText =
  HiLink "ProGrepText" "Directory"

grepSyntax :: Syntax
grepSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, path, ln, line, col, colon, text_, sync]
    links =
      [hlAsterisk, hlPath, hlLn, hlLine, hlCol, hlText]
