" Latexmk compiler integration.

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'latexmk'

setlocal makeprg=latexmk\ -interaction=nonstopmode\ -Werror\ -lualatex\ %

let &cpo = s:cpo_save
unlet s:cpo_save
