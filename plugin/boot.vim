let s:repo = fnamemodify(expand('<sfile>'), ':p:h:h')
let s:exe = s:repo . '/result/bin/proteome'
let s:build_cmd = [
  \ 'nix',
  \ '--option', 'extra-substituters', 'https://tek.cachix.org',
  \ '--option', 'extra-trusted-public-keys', 'tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=',
  \ 'build', '.#proteome',
  \ ]
let s:errors = []

function! s:run(exe) abort "{{{
  call jobstart([a:exe] + get(g:, 'proteome_cli_args', []), { 'rpc': v:true, 'cwd': s:repo, })
endfunction "}}}

function! s:error(msg) abort "{{{
  call nvim_echo(
        \ [[a:msg . ":\n", 'Error']] +
        \ map(s:errors, { i, s -> [s, 'Error'] }),
        \ v:false, {})
endfunction "}}}

function! s:built(code) abort "{{{
  if a:code == 0
    call s:run(s:exe)
  else
    call s:error('Failed to build proteome')
  endif
endfunction "}}}

function! s:nix_build() abort "{{{
  call jobstart(s:build_cmd, {
        \ 'cwd': s:repo,
        \ 'on_exit': { i, code, n -> s:built(code) },
        \ 'on_stderr': { i, data, n -> s:stderr(data) },
        \ })
endfunction "}}}

function! s:fetched(code) abort "{{{
  if a:code == 0
    call system(['chmod', '+x',  s:gh_exe])
    call s:run(s:gh_exe)
  else
    call s:error('Failed to fetch the proteome executable from github')
  endif
endfunction "}}}

function! s:stderr(data) abort "{{{
  call extend(s:errors, a:data)
endfunction "}}}

function! s:fetch_bin() abort "{{{
  call jobstart(s:fetch_cmd, {
        \ 'cwd': s:repo,
        \ 'on_exit': { i, code, n -> s:fetched(code) },
        \ 'on_stderr': { i, data, n -> s:stderr(data) },
        \ })
endfunction "}}}

let s:gh_exe = s:repo . '/github-exe'
let s:fetch_cmd = [
  \ 'curl',
  \ '--no-progress-meter',
  \ '--location',
  \ '--create-dirs',
  \ '--output',
  \ s:gh_exe,
  \ 'https://github.com/tek/proteome/releases/download/latest/proteome'
  \ ]

if filereadable(s:exe)
  call s:run(s:exe)
elseif filereadable(s:gh_exe)
  call s:run(s:gh_exe)
else
  if executable('nix') && !get(g:, 'proteome_fetch_bin', 0)
    echo 'Building proteome...'
    call s:nix_build()
  else
    echo 'Fetching the proteome executable from github...'
    call s:fetch_bin()
  endif
endif

