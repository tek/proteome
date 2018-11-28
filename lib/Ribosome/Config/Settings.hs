module Ribosome.Config.Settings(
  Setting (..),
  setting,
  settingE,
  updateSetting,
) where

import Neovim
import Ribosome.Data.Ribo (Ribo)
import qualified Ribosome.Data.Ribosome as R (name)

data Setting a =
  Setting {
    name :: String,
    prefix :: Bool,
    fallback :: Maybe a
  }

settingVariableName :: Bool -> String -> Ribo e String
settingVariableName False n = return n
settingVariableName True n = do
  pluginName <- fmap R.name $ ask
  return $ pluginName ++ "_" ++ n

settingE :: NvimObject a => Setting a -> Ribo e (Either String a)
settingE (Setting name' prefix' fallback') = do
  varName <- settingVariableName prefix' name'
  raw <- vim_get_var varName
  case raw of
    Right o -> fromObject' o
    Left a -> return $ case fallback' of
      Just fb -> Right fb
      Nothing -> Left $ show a

setting :: NvimObject a => Setting a -> Ribo e a
setting s = do
  raw <- settingE s
  case raw of
    Right o -> return o
    Left e -> fail e

updateSetting :: NvimObject a => Setting a -> a -> Ribo e ()
updateSetting (Setting name' prefix' _) a = do
  varName <- settingVariableName prefix' name'
  _ <- vim_set_var' varName (toObject a)
  return ()
