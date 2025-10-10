" Project management helpers.

function! TempSetBufOpt(opt, val, callback)
    let buf = bufnr('%')
    let prevval = getbufvar(buf, a:opt)
    call setbufvar(buf, a:opt, a:val)
    call a:callback()
    call setbufvar(buf, a:opt, prevval)
endfunction

function! TempGrep(prg, args)
    call TempSetBufOpt('&grepprg', a:prg, {-> execute('grep ' . a:args)})
    redraw!
endfunction

" Task management.
command! -nargs=0 -bar Tasks tabedit DONE | split DOING | split TODO
command! -nargs=* -complete=file Todos call TempGrep('todos', <q-args>)

" Notes.
command -nargs=0 Notes split | lcd $NOTES_DIR | tabedit $NOTES_DIR

" Git integration.
" TODO: replace Fugitive's :Ggrep with this?
command! -nargs=+ -complete=file GitGrep call TempGrep('git grep -n --column', <q-args>)

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis
