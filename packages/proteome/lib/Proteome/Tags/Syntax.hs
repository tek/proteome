module Proteome.Tags.Syntax where

import Exon (exon)
import Ribosome.Syntax (HiLink (HiLink), Syntax, build, link, match, prefix, (#>), (>-))

tagsSyntax :: Syntax
tagsSyntax =
  (#links <>~ [HiLink "Module" "Type"]) $ build $ prefix "ProTags" $
  match "Line" "^.*$" #> match "Name" [exon|🟣 \S\+|] >- link "Directory" (match "Package" [exon|📦 \S\+|]) >-
    link "Module" (match "Module" [exon|\S\+|])