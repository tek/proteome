let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let g:chromatin_rplugins = get(g:, 'g:chromatin_rplugins', []) + [{ 'name': 'proteome', 'spec': 'stack:' . s:dir }]
