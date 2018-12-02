import Neovim

import Proteome.Plugin (plugin)

main :: IO ()
main = neovim defaultConfig {plugins = [plugin]}
