{-# LANGUAGE TemplateHaskell #-}

module Proteome.Init
(
  initialize,
  env
) where

import Neovim

env :: Neovim startupEnv ()
env = return ()

initialize :: Neovim startupEnv ()
initialize = return ()
