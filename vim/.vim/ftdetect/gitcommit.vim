" =============================================================
" Description:  Improve Git Commit Message Editing
" File:         ~/.vim/ftdetect/gitcommit.vim
" =============================================================

autocmd BufReadPre COMMIT_EDITMSG call vivid#enable('committia.vim')
autocmd BufFilePre,BufFilePost __committia_status__ setlocal nocursorline tw=0
