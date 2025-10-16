" Project management helpers.

" Task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO

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
