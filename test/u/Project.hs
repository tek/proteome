module Project(
  flag,
  hask,
  cn,
  tp,
  l,
  fn,
  cil,
  createTestProject,
  prot,
  idr,
  ag,
  ti,
  ta,
  li,
  la,
) where

import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.Ribo (Ribo)
import qualified Proteome.Settings as S (projectBaseDirs)
import Proteome.Data.Project (
  ProjectName(ProjectName),
  ProjectType(ProjectType),
  ProjectLang(ProjectLang),
  )

flag :: String
flag = "flagellum"

hask :: String
hask = "haskell"

idr :: String
idr = "idris"

ag :: String
ag = "agda"

cil :: String
cil = "cilia"

prot :: String
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

createTestProject :: ProjectType -> ProjectName -> Ribo e ()
createTestProject (ProjectType tpe) (ProjectName name) = do
  bases <- setting S.projectBaseDirs
  liftIO $ createDirectoryIfMissing True (head bases </> tpe </> name)
