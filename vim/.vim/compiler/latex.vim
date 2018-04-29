" =============================================================
" Description:  LaTeX to PDF Compiler
" File:         ~/.vim/compiler/latex.vim
" =============================================================

if executable('latexmk')
    setlocal makeprg=latexmk\ -pdf\ %
elseif executable('pdflatex')
    setlocal makeprg=pdflatex\ %\ &&\ pdflatex\ %
endif
