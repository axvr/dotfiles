" =============================================================
" Description:  Detect and Set the Binary Filetype
" File:         ~/.vim/ftdetect/binary.vim
" =============================================================

autocmd! BufRead,BufNewFile *.bin,*.exe,*.dll,*.so,*.o  setfiletype binary

