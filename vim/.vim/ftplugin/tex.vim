" =============================================================
" Description:  Set up Vim for Editing LaTeX Files
" File:         ~/.vim/ftplugin/tex.vim
" =============================================================

" TODO check `:h tex.vim`
setlocal nofoldenable 
setlocal makeprg=latexmk\ -pdf\ %
