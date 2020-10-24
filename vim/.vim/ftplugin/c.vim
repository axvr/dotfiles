" =============================================================
" Description:  C configuration for Vim.
" File:         ~/.vim/ftplugin/c.vim
" =============================================================

setlocal path+=/usr/include

" Default to GNU style.
call cstyle#gnu()

let g:c_gnu = 1
let g:c_comment_strings = 1
