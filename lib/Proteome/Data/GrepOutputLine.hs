{-# LANGUAGE DeriveAnyClass #-}

module Proteome.Data.GrepOutputLine where

data GrepOutputLine =
  GrepOutputLine {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int,
    _text :: Text
  }
  deriving (Eq, Show, Generic, MsgpackEncode)

makeClassy ''GrepOutputLine
