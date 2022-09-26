module Proteome.Test.Dirs where

import Path (Dir, Path, Rel, reldir)

flag :: Path Rel Dir
flag = [reldir|flagellum|]

hask :: Path Rel Dir
hask = [reldir|haskell|]

idr :: Path Rel Dir
idr = [reldir|idris|]

ag :: Path Rel Dir
ag = [reldir|agda|]

cil :: Path Rel Dir
cil = [reldir|cilia|]

prot :: Path Rel Dir
prot = [reldir|proteome|]
