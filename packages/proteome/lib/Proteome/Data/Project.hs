module Proteome.Data.Project where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Proteome.Data.ProjectLang (ProjectLang (ProjectLang))
import Proteome.Data.ProjectMetadata (ProjectMetadata)
import Proteome.Data.ProjectType (ProjectType (ProjectType))

data Project =
  Project {
    meta :: ProjectMetadata,
    types :: [ProjectType],
    lang :: Maybe ProjectLang,
    langs :: [ProjectLang]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

instance Default Project where
  def =
    Project def def def def

langOrType :: Maybe ProjectLang -> Maybe ProjectType -> Maybe ProjectLang
langOrType (Just lang') _ = Just lang'
langOrType Nothing (Just (ProjectType tpe')) = Just (ProjectLang tpe')
langOrType _ _ = Nothing
