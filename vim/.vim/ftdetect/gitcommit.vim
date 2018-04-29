" =============================================================
" Description:  Improve Git Commit Message Editing
" File:         ~/.vim/ftdetect/gitcommit.vim
" =============================================================

autocmd BufReadPre COMMIT_EDITMSG call vivid#enable('committia.vim')
