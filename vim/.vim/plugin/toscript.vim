" Summary: Quickly convert the current file into an executable script.
" Help:    :help axvr/to-script

" TODO: change this into a per-buffer 'b:axvr_shebang' option?
let g:axvr_ft2shebang = {
    \   'awk':        '#!/usr/bin/env -S awk -f',
    \   'bash':       '#!/usr/bin/env bash',
    \   'bass':       '#!/usr/bin/env bass',
    \   'bb':         '#!/usr/bin/env bb',
    \   'elixir':     '#!/usr/bin/env -S ERL_FLAGS=+B elixir',
    \   'erlang':     '#!/usr/bin/env escript',
    \   'execline':   '#!/usr/bin/env -S execlineb -W',
    \   'fish':       '#!/usr/bin/env fish',
    \   'javascript': '#!/usr/bin/env node',
    \   'julia':      '#!/usr/bin/env julia',
    \   'perl':       '#!/usr/bin/env perl',
    \   'python':     '#!/usr/bin/env python3',
    \   'scheme':     '#!/usr/bin/env -S csi -script',
    \   'sh':         '#!/bin/sh',
    \   'vim':        '#!/usr/bin/env vim -S'
    \ }

function! s:to_script(ft) abort
    let bufnr = bufnr('%')
    call setbufvar(bufnr, '&filetype', a:ft)

    let shebang = get(g:axvr_ft2shebang, a:ft, '')
    if !empty(shebang)
        call appendbufline(bufnr, 0, shebang)
        call appendbufline(bufnr, 1, '')
    else
        call axvr#Warn('No matching shebang identified for filetype: ' .. a:ft)
    endif

    if &modifiable && !empty(bufname(bufnr))
        update
    endif

    let fpath = expand('%:.')
    if filereadable(fpath)
        call system(['chmod', '+x', fpath])
    else
        call axvr#Warn('Unable to make executable as buffer is not a real file.')
    endif
endfunction

function! s:file_types(arglead, _cmdline, _curpos)
    return axvr#MatchFuzzy(sort(keys(g:axvr_ft2shebang)), a:arglead)
endfunction

command -bar -nargs=1 -complete=customlist,s:file_types ToScript call s:to_script(<f-args>)

" Rapidly start creating a new do-script.
nnoremap <leader>ds :<C-u> <bar> ToScript sh<home>edit<space>do/
nnoremap <leader>db :<C-u> <bar> ToScript bash<home>edit<space>do/
nnoremap <leader>de :<C-u> <bar> ToScript execline<home>edit<space>do/
