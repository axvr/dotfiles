" =============================================================
" Description:  Set up Vim for C# (.NET Core) Projects
" File:         ~/.vim/ftdetect/cs.vim
" =============================================================

" TODO create a function which will select a compiler

let g:OmniSharp_timeout = 5

autocmd FileType cs call vivid#enable('omnisharp-vim')
autocmd FileType cs,aspx.html,cshtml.html call vivid#enable('vim-csharp')

autocmd BufReadPre,BufNewFile *.cs setfiletype cs
autocmd BufRead,BufNewFile *.cs,*.cshtml,*.csproj,*.aspx,*.sln compiler dotnet
autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml
