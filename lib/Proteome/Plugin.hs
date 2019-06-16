module Proteome.Plugin where

import Data.Default.Class (Default(def))
import Neovim (CommandOption(CmdBang), Neovim, NeovimPlugin, Plugin, wrapPlugin)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (RpcDef, autocmd, cmd, name, riboPlugin, rpcHandler, rpcHandlerDef, sync)

import Proteome.Add (proAdd, proAddCmd)
import Proteome.BufEnter (bufEnter)
import Proteome.Config (proReadConfig)
import Proteome.Data.Env (Env, Proteome)
import Proteome.Data.Error (Error)
import Proteome.Diag (proDiag)
import Proteome.Grep (proGrep, proGrepIn)
import Proteome.Init (initialize, proteomeStage1, proteomeStage2, proteomeStage4)
import Proteome.Project.Activate (proNext, proPrev)
import Proteome.Quit (proQuit)
import Proteome.Save (proSave)
import Proteome.Tags (proTags)

handleError :: Error -> Proteome ()
handleError =
  reportError "proteome"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler sync 'proteomeStage1),
    $(rpcHandler sync 'proteomeStage2),
    $(rpcHandler sync 'proteomeStage4),
    $(rpcHandler (name "ProAddProject") 'proAdd),
    $(rpcHandler (cmd [CmdBang] . name "ProAdd") 'proAddCmd),
    $(rpcHandlerDef 'proSave),
    $(rpcHandlerDef 'proTags),
    $(rpcHandler (cmd []) 'proGrep),
    $(rpcHandler (cmd []) 'proGrepIn),
    $(rpcHandler (cmd []) 'proNext),
    $(rpcHandler (cmd []) 'proPrev),
    $(rpcHandlerDef 'proReadConfig),
    $(rpcHandler (cmd []) 'proDiag),
    $(rpcHandler (autocmd "BufEnter") 'bufEnter),
    $(rpcHandler (autocmd "BufWritePost") 'proSave),
    $(rpcHandler (autocmd "VimLeave" . sync) 'proQuit)
  ]


plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "proteome" env rpcHandlers def handleError def

plugin :: Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' =<< initialize
