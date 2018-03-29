" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftdetect/cs.vim
" =============================================================

autocmd FileType cs,aspx.html,cshtml.html call vivid#enable('vim-csharp')
let g:OmniSharp_server_path = expand('~/.omnisharp/run')
if has('unix') && system('uname -o | grep "^Cygwin$"') != ''
    let g:OmniSharp_translate_cygwin_wsl = 1
endif
autocmd FileType cs call vivid#enable('omnisharp-vim')
autocmd BufRead,BufNewFile *.cshtml setfiletype cshtml.html
autocmd BufRead,BufNewFile *.aspx   setfiletype aspx.html
autocmd BufRead,BufNewFile *.csproj setlocal filetype=csproj.xml

