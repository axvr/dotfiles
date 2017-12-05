" =============================================================
" Description:  Detect and Set the Encrypted Filetype
" File:         ~/.vim/ftdetect/encrypted.vim
" =============================================================

autocmd! BufRead,BufNewFile *.asc,*.gpg,*.pgp setfiletype encrypted

