" =============================================================
" Description:  Set up Vim for Editing LaTeX Files
" File:         ~/.vim/ftplugin/tex.vim
" =============================================================

if executable('latexmk')
    setlocal makeprg=latexmk\ -pdf\ %
elseif executable('pdflatex')
    setlocal makeprg=pdflatex\ %\ &&\ pdflatex\ %
endif
