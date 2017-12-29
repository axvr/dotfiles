" =============================================================
" Description:  Set up Vim for Viewing and Editing Binary Files
" File:         ~/.vim/ftplugin/binary.vim
" =============================================================

augroup Binary
    au!
    au BufReadPre   binary let &bin=1
    au BufReadPost  binary if &bin | %!xxd
    au BufReadPost  binary set ft=xxd | endif
    au BufWritePre  binary if &bin | %!xxd -r
    au BufWritePre  binary endif
    au BufWritePost binary if &bin | %!xxd
    au BufWritePost binary set nomod | endif
augroup END

