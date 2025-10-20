let b:omni_syntax_ignorecase = 1
setlocal nojoinspaces
setlocal iskeyword+=&,:

nnoremap <buffer> <C-K> :silent !hyperspec <cword><CR>
