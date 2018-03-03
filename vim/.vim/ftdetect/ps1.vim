" =============================================================
" Description:  Set up Vim for Editing PowerShell files
" File:         ~/.vim/ftdetect/ps1.vim
" =============================================================

augroup PowerShell
    autocmd!
    autocmd FileType ps1 call vivid#enable('vim-ps1')
    autocmd BufRead,BufNewFile *.ps1,*.psc1,*.ps1xml setfiletype ps1
augroup END

