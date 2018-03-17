" =============================================================
" Description:  Set up Vim for Editing LaTeX Files
" File:         ~/.vim/ftplugin/tex.vim
" =============================================================

if executable('latexmk') || executable('pdflatex')
    setlocal makeprg=latexmk\ -pdf\ %
endif
