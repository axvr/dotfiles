" Project management helpers.

" Match column numbers in 'grepformat'
set grepformat^=%f:%l:%c:%m

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
command! -nargs=0 -bar Tasks tabe DONE | split DOING | split TODO
command! -nargs=* -complete=file Todos call TempGrep('todos', <q-args>)

" Git integration.
command! -nargs=+ -complete=file GitGrep call TempGrep('git grep -n --column', <q-args>)
com! -nargs=? -range GitBlame ec join(systemlist("git -C ".shellescape(expand('%:p:h')).
            \ " blame -L <line1>,<line2> <args> -- ".expand('%:t')),"\n")

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig
            \ <mods> new
            \ | read ++edit # | 0d_ | diffthis
            \ | setl buftype=nofile readonly noswapfile bufhidden=wipe nobuflisted nomodifiable
            \ | exe 'setfiletype ' . getbufvar('#', '&l:filetype')
            \ | exe 'silent file [Diff] ' . bufname('#')
            \ | wincmd p | diffthis

" Create parent directories on buffer write if they don't exist.
function! s:create_parent_dirs()
    let dir = expand("%:p:h")
    if !isdirectory(dir) && confirm('Create directory "'.dir.'"?', "&Yes\n&No") == 1
        call mkdir(dir, 'p')
    endif
endfunction

augroup create_parent_dirs
    autocmd!
    autocmd BufWritePre * :call <SID>create_parent_dirs()
augroup END

function! s:trim_whitespace()
    let view = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(view)
endfunction

augroup trim_trailing_whitespace
    autocmd BufWritePre * call s:trim_whitespace()
augroup END
