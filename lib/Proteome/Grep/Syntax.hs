{-# LANGUAGE QuasiQuotes #-}

module Proteome.Grep.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.Syntax (
  HiLink(HiLink),
  Syntax(Syntax),
  SyntaxItem(siOptions, siParams),
  syntaxMatch,
  syntaxVerbatim,
  )
import Text.RawString.QQ (r)

lineNumber :: Text
lineNumber =
  "\57505"

path :: SyntaxItem
path =
  item { siParams = params }
  where
    item = syntaxMatch "ProGrepPath" ([r|^.*\ze |] <> lineNumber)
    params = Map.fromList [("nextgroup", "ProGrepLN")]

ln :: SyntaxItem
ln =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepLN" lineNumber
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepLine")]

line :: SyntaxItem
line =
  item { siParams = params }
  where
    item = syntaxMatch "ProGrepLine" [r|\d\+\ze\/|]
    params = Map.fromList [("nextgroup", "ProGrepCol")]

col :: SyntaxItem
col =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProGrepCol" [r|\d\+|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProGrepText")]

text :: SyntaxItem
text =
  item { siOptions = options }
  where
    item = syntaxMatch "ProGrepText" [r|.\+|]
    options = ["skipwhite"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlPath :: HiLink
hlPath =
  HiLink "ProGrepPath" "Directory"

hlLn :: HiLink
hlLn =
  HiLink "ProGrepLN" "Statement"

hlLine :: HiLink
hlLine =
  HiLink "ProGrepLine" "Constant"

hlCol :: HiLink
hlCol =
  HiLink "ProGrepCol" "Constant"

grepSyntax :: Syntax
grepSyntax =
  Syntax items [] links
  where
    items =
      [path, ln, line, col, sync, text]
    links =
      [hlPath, hlLn, hlLine, hlCol]
