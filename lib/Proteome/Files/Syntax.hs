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
    params = Map.fromList [("nextgroup", "ProFilesName")]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProFilesAsterisk" "Todo"

hlName :: HiLink
hlName =
  HiLink "ProFilesName" "Type"

filesSyntax :: Syntax
filesSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, sync]
    links =
      [hlAsterisk, hlName]
