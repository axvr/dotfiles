" =============================================================
" Description:  Better LaTeX Document Editing
" File:         ~/.vim/ftdetect/tex.vim
" =============================================================

autocmd BufNewFile *.tex %d|r ~/.vim/skeleton/skeleton.tex|1d
