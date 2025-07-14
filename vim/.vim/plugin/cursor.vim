" When opening a file, jump to the last known cursor position.
augroup cursor_restore
    autocmd!
    autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " Reset cursor position to top of file for these files.
    autocmd BufReadPost */.git/* normal! gg0
augroup END

