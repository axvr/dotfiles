highlight! def link schemeParentheses Delimiter

" Display `lambda` as a lambda character.
syntax keyword schemeSyntax lambda conceal cchar=λ
setlocal conceallevel=1
