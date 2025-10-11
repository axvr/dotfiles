" Project management helpers.

" Faster `:find` for significantly faster searching and fuzzy search.
nnoremap <leader>f :find<space>
function! s:find_fuzzy(cmdarg, _) abort
    return axvr#FuzzyMatch(systemlist('fd -HE .git -d 8 .'), a:cmdarg)
endfunction
if executable('fd') | set findfunc=s:find_fuzzy | endif

" Task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO
command! -nargs=* -complete=file_in_path Todos call axvr#TempGrep('todos', <q-args>)

" Notes.
command -nargs=0 Notes split | lcd $NOTES_DIR | tabedit $NOTES_DIR

" Git integration.
" TODO: replace Fugitive's :Ggrep with this?
command! -nargs=+ -complete=file_in_path GitGrep call axvr#TempGrep('git grep -n --column', <q-args>)

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis
