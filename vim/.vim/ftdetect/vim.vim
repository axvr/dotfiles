" =============================================================
" Description:  Improved Vim Script Editing
" File:         ~/.vim/ftdetect/vim.vim
" =============================================================

autocmd BufNewFile *.vim %d|r ~/.vim/skeleton/skel.vim|1d
