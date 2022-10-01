module Proteome.Data.CurrentTag where

import qualified Data.List.NonEmpty.Zipper as Zipper
import Data.List.NonEmpty.Zipper (Zipper)
import Path (Abs, File, Path)

import Proteome.Tags.State (TagLoc)

data CurrentTag =
  CurrentTag {
    name :: Text,
    locations :: Zipper (TagLoc (Path Abs File)),
    bufferWasLoaded :: Bool
  }
  deriving stock (Eq, Show, Generic)

pattern CurrentLoc :: TagLoc (Path Abs File) -> CurrentTag
pattern CurrentLoc loc <- (Zipper.current . locations -> loc)
{-# complete CurrentLoc #-}

cycleLoc ::
  Zipper (TagLoc (Path Abs File)) ->
  Zipper (TagLoc (Path Abs File))
cycleLoc locs =
  fromMaybe (Zipper.start locs) (Zipper.right locs)
