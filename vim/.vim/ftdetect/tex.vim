" =============================================================
" Description:  Better LaTeX Document Editing
" File:         ~/.vim/ftdetect/tex.vim
" =============================================================

let g:tex_flavor = "latex"
autocmd BufNewFile *.tex %d|r ~/.vim/skeleton/skel.tex|1d
