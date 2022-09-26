module Proteome.Test.Project where

import Path (parseRelDir, (</>))
import Path.IO (ensureDir)
import Polysemy.Test (TestError (TestError))
import Ribosome (Settings)
import qualified Ribosome.Settings as Settings

import Proteome.Data.ProjectConfig (ProjectConfig (ProjectConfig))
import Proteome.Data.ProjectLang (ProjectLang (ProjectLang))
import Proteome.Data.ProjectName (ProjectName (ProjectName))
import Proteome.Data.ProjectType (ProjectType (ProjectType))
import qualified Proteome.Settings as Settings (projectConfig)

flag :: Text
flag = "flagellum"

hask :: Text
hask = "haskell"

idr :: Text
idr = "idris"

ag :: Text
ag = "agda"

cil :: Text
cil = "cilia"

prot :: Text
prot = "proteome"

fn :: ProjectName
fn = ProjectName flag

cn :: ProjectName
cn = ProjectName cil

tp :: ProjectType
tp = ProjectType hask

ti :: ProjectType
ti = ProjectType idr

ta :: ProjectType
ta = ProjectType ag

l :: ProjectLang
l = ProjectLang hask

li :: ProjectLang
li = ProjectLang idr

la :: ProjectLang
la = ProjectLang ag

createTestProject ::
  Members [Settings, Fail, Error TestError, Embed IO] r =>
  ProjectType ->
  ProjectName ->
  Sem r ()
createTestProject (ProjectType tpe) (ProjectName name) = do
  mapError (TestError . show) do
    (ProjectConfig (base : _) _ _ _ _ _ _) <- Settings.get Settings.projectConfig
    typePath <- fromEither (parseRelDir (toString tpe))
    namePath <- fromEither (parseRelDir (toString name))
    ensureDir (base </> typePath </> namePath)
