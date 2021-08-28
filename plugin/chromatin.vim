let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let conf = {
\ 'name': 'proteome',
\ 'spec': 'flake:path:' . s:dir,
\ 'dev': v:true,
\ 'debug': get(g:, 'proteome_debug', 0)
\ }
let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [conf]
