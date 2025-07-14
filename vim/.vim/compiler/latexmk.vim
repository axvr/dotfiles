" Latexmk compiler integration.

let current_compiler = 'latexmk'
setlocal makeprg=latexmk\ -interaction=nonstopmode\ -Werror\ -lualatex\ %
