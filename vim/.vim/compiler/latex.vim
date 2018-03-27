" =============================================================
" Description:  LaTeX to PDF Compiler
" File:         ~/.vim/compiler/latex.vim
" =============================================================

let current_compiler = 'latex'
let s:cpo_save = &cpo
set cpo&vim

compiler tex

if executable('latexmk')
    setlocal makeprg=latexmk\ -pdf\ %
elseif executable('pdflatex')
    setlocal makeprg=pdflatex\ %\ &&\ pdflatex\ %
endif

let &cpo = s:cpo_save
unlet s:cpo_save

