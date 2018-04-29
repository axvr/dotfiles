" =============================================================
" Description:  Set up Vim for Editing PowerShell Scripts
" File:         ~/.vim/ftdetect/powershell.vim
" =============================================================

autocmd BufRead,BufNewFile *.ps1,*.psd1,*.psm1,*.pssc setfiletype powershell
autocmd BufRead,BufNewFile *.ps1xml,*.psc1,*.cdxml    setfiletype xml
