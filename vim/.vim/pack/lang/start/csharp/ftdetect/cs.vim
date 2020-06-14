" =============================================================
" Description:  C# file type detection.
" File:         ftdetect/cs.vim
" =============================================================

let g:OmniSharp_server_stdio = 1
let g:OmniSharp_highlighting = 2
let g:OmniSharp_popup_options = { 'highlight': 'Pmenu', 'padding': [1] }
let g:omnicomplete_fetch_full_documentation = 1

autocmd BufReadPre,BufNewFile *.cs packadd omnisharp | setfiletype cs
autocmd BufReadPre,BufNewFile NuGet.config setfiletype xml
autocmd BufRead,BufNewFile *.cs,*.cshtml,*.csproj,*.aspx,*.sln compiler dotnet
autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml
autocmd FileType csproj.xml setlocal shiftwidth=2 softtabstop=2 expandtab
