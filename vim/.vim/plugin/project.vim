" Project management helpers.

" Faster `:find` and `:grep`.
nnoremap <leader>f :find<space>
nnoremap <leader>/ :silent grep ''<left>
function! s:find_fuzzy(cmdarg, _) abort
    return axvr#MatchFuzzy(systemlist('fd -HE .git -d 8 .'), a:cmdarg)
endfunction
if executable('fd') | set findfunc=s:find_fuzzy | endif
if executable('rg') | set grepprg=rg\ --vimgrep\ --smart-case\ --hidden\ -g\ '!.git/*' | endif

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

" Task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO
command! -nargs=* -bang -complete=file_in_path Todos
            \ exec 'GrepWith'.<q-bang> 'todos' <q-args>

" Notes.
command -nargs=0 Notes <mods> tabedit $NOTES_DIR/index.md | silent lcd $NOTES_DIR | arglocal

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis
