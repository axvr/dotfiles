highlight! def link schemeParentheses Delimiter

" Display `lambda` as a lambda character.
syntax keyword schemeSyntax lambda conceal cchar=λ
highlight! link Conceal schemeSyntax
setlocal conceallevel=1
