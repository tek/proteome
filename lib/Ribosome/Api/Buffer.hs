module Ribosome.Api.Buffer(
  edit,
  buflisted,
) where

import Neovim (Neovim, vim_command', Buffer, NvimObject(..), vim_call_function', Object, buffer_get_number')
import Ribosome.Data.Ribo (Ribo)

edit :: FilePath -> Ribo e ()
edit path = vim_command' $ "silent! edit " ++ path

nvimCallBool :: String -> [Object] -> Neovim e Bool
nvimCallBool fun args =
  vim_call_function' fun args >>= fromObject'

buflisted :: Buffer -> Neovim e Bool
buflisted buf = do
  num <- buffer_get_number' buf
  nvimCallBool "buflisted" [toObject num]
