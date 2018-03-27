" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftdetect/cs.vim
" =============================================================

augroup DotNet
    autocmd!
    autocmd FileType cs,aspx.html,cshtml.html call vivid#enable('vim-csharp')
    " FIXME OmniSharp Roslyn server will not run
    "let g:OmniSharp_server_type = 'v1'
    let g:OmniSharp_server_path = expand('~/.omnisharp/omnisharp/OmniSharp.exe')
    autocmd FileType cs call vivid#enable('omnisharp-vim')
    autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
    autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
    autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml
augroup END

