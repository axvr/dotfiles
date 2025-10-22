" Summary: Better searching and search tools.
" Help:    N/A

" Faster `:find` and `:grep`.
function! s:find_fuzzy(cmdarg, _) abort
    return axvr#MatchFuzzy(systemlist('fd -HE .git -d 8 .'), a:cmdarg)
endfunction
if executable('fd') | set findfunc=s:find_fuzzy | endif
if executable('rg') | set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ -g\ '!.git/*' | endif

nnoremap <leader>/ :silent grep ''<left>
nnoremap <leader>f :find<space>
nnoremap <leader>b :buffer<space>

" Quickly use alternate grep-style output tools.
"   :GrepWith todos % | grep src
"   :GrepWith git markers
"   :GrepWithAdd todos %
"   :LgrepWith git markers
"   :LgrepaddWith todos %
command! -nargs=+ -bang -complete=shellcmdline GrepWith
    \ call axvr#GrepWith('exec', <q-args>, {"jump": empty(<q-bang>)})
command! -nargs=+ -bang -complete=shellcmdline GrepaddWith
    \ call axvr#GrepWith('exec', <q-args>, {"jump": empty(<q-bang>), "add": 1})
command! -nargs=+ -bang -complete=shellcmdline LgrepWith
    \ call axvr#GrepWith('exec', <q-args>, {"jump": empty(<q-bang>), "loc": 1})
command! -nargs=+ -bang -complete=shellcmdline LgrepaddWith
    \ call axvr#GrepWith('exec', <q-args>, {"jump": empty(<q-bang>), "add": 1, "loc": 1})

command! -nargs=* -bang -complete=file_in_path Todos
    \ exec 'GrepWith'..<q-bang> 'todos' <q-args>
