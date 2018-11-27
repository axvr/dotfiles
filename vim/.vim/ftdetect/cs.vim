" =============================================================
" Description:  Set up Vim for C# (.NET Core) Projects
" File:         ~/.vim/ftdetect/cs.vim
" =============================================================

if v:version <= 800
    packadd vim-cs
endif

let g:OmniSharp_timeout = 5

autocmd BufReadPre,BufNewFile *.cs setfiletype cs
autocmd BufRead,BufNewFile *.cs,*.cshtml,*.csproj,*.aspx,*.sln compiler dotnet
autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml
autocmd FileType csproj.xml setlocal shiftwidth=2 softtabstop=2
