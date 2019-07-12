{-# LANGUAGE QuasiQuotes #-}

module Proteome.Files.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.Syntax (
  HiLink(HiLink),
  Syntax(Syntax),
  SyntaxItem(siOptions, siParams),
  syntaxMatch,
  syntaxVerbatim,
  )
import Text.RawString.QQ (r)

asterisk :: SyntaxItem
asterisk =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProFilesAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProFilesNumber")]

number :: SyntaxItem
number =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "ProFilesNumber" [r|\d\+|]
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProFilesName")]

line :: SyntaxItem
line =
  item { siOptions = options }
  where
    item = syntaxMatch "ProFilesName" ".*$"
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProFilesAsterisk" "Todo"

hlNumber :: HiLink
hlNumber =
  HiLink "ProFilesNumber" "Directory"

hlName :: HiLink
hlName =
  HiLink "ProFilesName" "Type"

filesSyntax :: Syntax
filesSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, number, line, sync]
    links =
      [hlAsterisk, hlNumber, hlName]
