" =============================================================
" Description:  Improve Git Commit Message Editing
" File:         ~/.vim/ftdetect/gitcommit.vim
" =============================================================

autocmd BufReadPre COMMIT_EDITMSG call vivid#enable('committia.vim')
autocmd FileType git setlocal nocursorline
autocmd FileType gitcommit setlocal spell
