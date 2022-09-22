module Proteome.Buffers.Syntax where

import qualified Data.Map.Strict as Map (fromList)
import Ribosome.Data.SyntaxItem (SyntaxItem (options, params))
import Ribosome.Syntax (HiLink (HiLink), Syntax (Syntax), syntaxMatch, syntaxVerbatim)
import Text.RawString.QQ (r)

asterisk :: SyntaxItem
asterisk =
  item {options, params}
  where
    item = syntaxMatch "ProBuffersAsterisk" [r|^ \*|]
    options = ["skipwhite"]
    params = Map.fromList [("nextgroup", "ProBuffersNumber")]

number :: SyntaxItem
number =
  item {options, params}
  where
    item = syntaxMatch "ProBuffersNumber" [r|\d\+|]
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "ProBuffersName")]

line :: SyntaxItem
line =
  item {options}
  where
    item = syntaxMatch "ProBuffersName" ".*$"
    options = ["contained"]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=1"

hlAsterisk :: HiLink
hlAsterisk =
  HiLink "ProBuffersAsterisk" "Todo"

hlNumber :: HiLink
hlNumber =
  HiLink "ProBuffersNumber" "Directory"

hlName :: HiLink
hlName =
  HiLink "ProBuffersName" "Type"

buffersSyntax :: Syntax
buffersSyntax =
  Syntax items [] links
  where
    items =
      [asterisk, number, line, sync]
    links =
      [hlAsterisk, hlNumber, hlName]
