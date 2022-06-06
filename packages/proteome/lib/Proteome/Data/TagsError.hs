module Proteome.Data.TagsError where

import Exon (exon)
import Log (Severity (Warn))
import Ribosome (ErrorMessage (ErrorMessage), SettingError, ToErrorMessage (toErrorMessage))
import qualified Ribosome.Host.Data.HandlerError as ErrorMessage

data TagsError =
  Process Text
  |
  TempName
  |
  Setting SettingError
  deriving stock (Eq, Show)

instance ToErrorMessage TagsError where
  toErrorMessage = \case
    TempName ->
      ErrorMessage "failed to create temp dir for tags process" ["TagsError.TempName"] Warn
    Process msg ->
      ErrorMessage "tags process failed" ["TagsError.Process", msg] Warn
    Setting e ->
      ErrorMessage [exon|tags failed: #{ErrorMessage.user (toErrorMessage e)}|] ["TagsError.Setting:", show e] Warn
