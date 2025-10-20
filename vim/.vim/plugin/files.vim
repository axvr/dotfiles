" Replace netrw with dirvish.
let [g:netrw_banner, g:loaded_netrw, g:loaded_netrwPlugin] = [0, 1, 1]
let g:dirvish_mode = ':sort | silent! g,\v/\.(DS_Store|git/)$,d _'
packadd dirvish

function! s:create_parent_dirs()
    let dir = expand("%:p:h")
    if ! isdirectory(dir) && axvr#YN('Create directory?')
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

augroup axvr/file_utils
    autocmd!
    autocmd BufWritePre * call s:create_parent_dirs()
    autocmd BufWritePre * call s:trim_whitespace()
    " Don't trim whitespace on diff files as it breaks syntax highlighting.
    autocmd FileType gitcommit,diff let b:no_whitespace_trim = 1
augroup END

" When opening a file, jump to the last known cursor position.
augroup axvr/cursor_restore
    autocmd!
    autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " Reset cursor position to top of file for these files.
    autocmd BufReadPost */.git/* normal! gg0
augroup END
