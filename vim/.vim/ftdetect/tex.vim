" =============================================================
" Description:  Better LaTeX Document Editing
" File:         ~/.vim/ftdetect/tex.vim
" =============================================================

autocmd BufNewFile *.tex %d|r ~/.vim/skeleton/skel.tex|1d
