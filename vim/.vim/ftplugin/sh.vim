" =============================================================
" Description:  Set up Vim for Editing Shell Files
" File:         ~/.vim/ftplugin/sh.vim
" =============================================================

" Set up Vim make and quickfix
setlocal makeprg=shellcheck\ -f\ gcc\ %
setlocal errorformat=%f:%l:%c:\ %trror:\ %m
setlocal errorformat+=%f:%l:%c:\ %tarning:\ %m
setlocal errorformat+=%f:%l:%c:\ note:\ %m
setlocal errorformat+=%f:%l:%c:\ %m
