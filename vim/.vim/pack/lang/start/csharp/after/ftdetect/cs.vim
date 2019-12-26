" =============================================================
" Description:  C# file type detection.
" File:         after/ftdetect/cs.vim
" =============================================================

let g:OmniSharp_server_stdio = 1
let g:OmniSharp_highlight_types = 1

autocmd BufReadPre,BufNewFile *.cs setfiletype cs
autocmd BufRead,BufNewFile *.cs,*.cshtml,*.csproj,*.aspx,*.sln compiler dotnet
autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml
autocmd FileType csproj.xml setlocal shiftwidth=2 softtabstop=2
