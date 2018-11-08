" =============================================================
" Description:  Set up Vim for Editing PowerShell Scripts
" File:         ~/.vim/ftplugin/powershell.vim
" =============================================================

setlocal commentstring=#%s

" PowerShell Syntax highlighting
let s:file = '~/.vim/syntax/powershell.vim'
let s:url = 'https://raw.githubusercontent.com/PProvost/vim-ps1/master/syntax/ps1.vim'

if !filereadable(expand(s:file)) && executable('curl')
    call system('curl --create-dirs "'.s:url.'" -o "'.expand(s:file).'"')
    echomsg 'Installed PowerShell syntax highlighting'
    exe 'source '.s:file
endif
