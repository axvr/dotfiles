" Summary: Provides just the good mappings from vim-unimpaired.
" Help:    N/A

" Neovim already has these built in.
if has('nvim') | finish | endif

" Argument list.
nnoremap <silent> [a     :<C-u>exec v:count 'previous'<CR>
nnoremap <silent> ]a     :<C-u>exec v:count 'next'<CR>
nnoremap          [A     :<C-u>first<CR>
nnoremap          ]A     :<C-u>last<CR>

" Buffer list.
nnoremap <silent> [b     :<C-u>exec v:count 'bprevious'<CR>
nnoremap <silent> ]b     :<C-u>exec v:count 'bnext'<CR>
nnoremap          [B     :<C-u>bfirst<CR>
nnoremap          ]B     :<C-u>blast<CR>

" Location list.
nnoremap <silent> [l     :<C-u>exec v:count 'lprevious'<CR>
nnoremap <silent> ]l     :<C-u>exec v:count 'lnext'<CR>
nnoremap          [L     :<C-u>lfirst<CR>
nnoremap          ]L     :<C-u>llast<CR>
nnoremap <silent> [<C-L> :<C-u>exec v:count 'lpfile' <CR>
nnoremap <silent> ]<C-L> :<C-u>exec v:count 'lnfile'<CR>

" Quickfix list.
nnoremap <silent> [q     :<C-u>exec v:count 'cprevious'<CR>
nnoremap <silent> ]q     :<C-u>exec v:count 'cnext'<CR>
nnoremap          [Q     :<C-u>cfirst<CR>
nnoremap          ]Q     :<C-u>clast<CR>
nnoremap <silent> [<C-Q> :<C-u>exec v:count 'cpfile'<CR>
nnoremap <silent> ]<C-Q> :<C-u>exec v:count 'cnfile'<CR>

" Tag list.
nnoremap <silent> [t     :<C-u>exec v:count 'tprevious'<CR>
nnoremap <silent> ]t     :<C-u>exec v:count 'tnext'<CR>
nnoremap          [T     :<C-u>exec v:count 'trewind'<CR>
nnoremap          ]T     :<C-u>tlast<CR>
nnoremap <silent> [<C-T> :<C-u>exec v:count 'ptprevious'<CR>
nnoremap <silent> ]<C-T> :<C-u>exec v:count 'ptnext'<CR>

" Line operations.
nnoremap <silent> ]<Space> :<C-u>ApartCall appendbufline('%', line('.'), '')<CR>
nnoremap <silent> [<Space> :<C-u>ApartCall appendbufline('%', line('.') - 1, '')<CR>
