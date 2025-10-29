" Summary: Quickly convert the current file into an executable script.
" Help:    :help axvr/to-script

let g:axvr_shebang = {
    \   'awk':        '#!/usr/bin/env -S awk -f',
    \   'bash':       ['#!/usr/bin/env bash', '', 'set -eo pipefail'],
    \   'bass':       '#!/usr/bin/env bass',
    \   'bb':         '#!/usr/bin/env bb',
    \   'elixir':     '#!/usr/bin/env -S ERL_FLAGS=+B elixir',
    \   'erlang':     '#!/usr/bin/env escript',
    \   'execline':   '#!/usr/bin/env -S execlineb -W',
    \   'fish':       '#!/usr/bin/env fish',
    \   'javascript': ['#!/usr/bin/env node', '', '"use strict";'],
    \   'julia':      '#!/usr/bin/env julia',
    \   'perl':       ['#!/usr/bin/env perl', '', 'use v5.34.0;', 'use strict;', 'use warnings;'],
    \   'python':     '#!/usr/bin/env python3',
    \   'scheme':     '#!/usr/bin/env -S csi -script',
    \   'sh':         ['#!/bin/sh', '', 'set -e'],
    \   'vim':        '#!/usr/bin/env vim -S'
    \ }

function! s:to_script(ft) abort
    let bufnr = bufnr('%')
    call setbufvar(bufnr, '&filetype', a:ft)

    let shebang = get(g:axvr_shebang, a:ft, [])
    let shebang = type(shebang) == v:t_string ? [shebang] : shebang
    if !empty(shebang)
        let shebang += ['']
        for i in range(0, len(shebang) - 1)
            call appendbufline(bufnr, i, shebang[i])
        endfor
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
    return axvr#MatchFuzzy(sort(keys(g:axvr_shebang)), a:arglead)
endfunction

command -bar -nargs=1 -complete=customlist,s:file_types ToScript call s:to_script(<f-args>)

" Rapidly start creating a new do-script.
nnoremap <leader>ds :<C-u> <bar> ToScript sh<home>edit<space>do/
nnoremap <leader>db :<C-u> <bar> ToScript bash<home>edit<space>do/
nnoremap <leader>de :<C-u> <bar> ToScript execline<home>edit<space>do/
