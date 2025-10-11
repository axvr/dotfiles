" Project management helpers.

" Faster `:find` and `:grep`.
nnoremap <leader>f :find<space>
nnoremap <leader>g :silent grep ''<left>
function! s:find_fuzzy(cmdarg, _) abort
    return axvr#FuzzyMatch(systemlist('fd -HE .git -d 8 .'), a:cmdarg)
endfunction
if executable('fd') | set findfunc=s:find_fuzzy | endif
if executable('rg') | set grepprg=rg\ --vimgrep\ --hidden\ -g\ '!.git/*' | endif

" Task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO
command! -nargs=* -complete=file_in_path Todos call axvr#TempGrep('todos', <q-args>)

" Notes.
command -nargs=0 Notes split | lcd $NOTES_DIR | tabedit $NOTES_DIR

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis
