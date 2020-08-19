augroup Scheme
    autocmd!
    autocmd BufRead,BufNewFile *.sxml setfiletype scheme
    autocmd BufRead,BufNewFile *.ss,gerbil.pkg setlocal ft=gerbil.scheme
    autocmd BufRead,BufNewFile *.scm,*.sld setlocal ft=chicken.scheme
augroup END
