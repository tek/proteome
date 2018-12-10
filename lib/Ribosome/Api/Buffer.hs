module Ribosome.Api.Buffer(
  edit,
) where

import Neovim (vim_command')
import Ribosome.Data.Ribo (Ribo)

edit :: FilePath -> Ribo e ()
edit path = vim_command' $ "silent! edit " ++ path
