let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [{ 'name': 'proteome', 'spec': 'stack:' . s:dir, 'dev': v:true, 'debug': get(g:, 'proteome_debug', 0) }]
