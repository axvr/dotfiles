" =============================================================
" Description:  Improved Vim Script Editing
" File:         ~/.vim/ftdetect/vim.vim
" =============================================================

autocmd BufNewFile *.vim %d|r ~/.vim/skeleton/skeleton.vim|1d
