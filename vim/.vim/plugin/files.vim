function! s:create_parent_dirs()
    let dir = expand("%:p:h")
    if ! isdirectory(dir) && confirm('Create directory "'.dir.'"?', "&Yes\n&No", 0, 'Question') == 1
        call mkdir(dir, 'p')
    endif
endfunction

function! s:trim_whitespace()
    if ! get(b:, 'no_whitespace_trim')
        let view = winsaveview()
        keeppatterns %s/\s\+$//e
        call winrestview(view)
    endif
endfunction

augroup file_utils
    autocmd!
    autocmd BufWritePre * call s:create_parent_dirs()
    autocmd BufWritePre * call s:trim_whitespace()
    " Don't trim whitespace on diff files as it breaks syntax highlighting.
    autocmd FileType gitcommit,diff let b:no_whitespace_trim = 1
augroup END
