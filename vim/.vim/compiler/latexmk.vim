" =============================================================
" Description:  Latexmk compiler integration.
" File:         ~/.vim/compiler/latexmk.vim
" =============================================================

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'latexmk'

setlocal mp=latexmk\ -pdf\ %

let &cpo = s:cpo_save
unlet s:cpo_save
