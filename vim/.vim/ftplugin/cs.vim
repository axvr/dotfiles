" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

setlocal commentstring=//%s
setlocal completeopt& completeopt-=preview

if vivid#enabled('omnisharp-vim')
    " C# documentation (use OmniSharp instead of '&keywordprg')
    nnoremap <buffer><silent> K :<C-u>OmniSharpDocumentation<CR>:wincmd P<CR>

    " Auto type lookup
    " if get(g:, 'dotnet_disable_auto_type_lookup') != 1
    "     autocmd! CursorHold *.cs :OmniSharpTypeLookup
    "     setlocal updatetime=1000
    " endif
endif
