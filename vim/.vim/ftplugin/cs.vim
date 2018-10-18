" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

setlocal commentstring=//%s
setlocal completeopt& completeopt-=preview
setlocal textwidth=0

syn region csAttribute start="^\s*\[" end="\]\s*" contains=csString, csVerbatimString, csCharacter, csNumber, csType
hi def link csAttribute PreProc

if vivid#enabled('omnisharp-vim')
    " C# documentation (use OmniSharp instead of '&keywordprg')
    nnoremap <buffer><silent> K :<C-u>OmniSharpDocumentation<CR>:wincmd P<CR>

    nnoremap <buffer> <localleader>oa :<C-u>OmniSharpGetCodeActions<CR>
    nnoremap <buffer> <localleader>ou :<C-u>OmniSharpFixUsings<CR>
    nnoremap <buffer> <localleader>ot :<C-u>OmniSharpTypeLookup<CR>

    nnoremap <buffer><silent> <C-]> :<C-u>OmniSharpGotoDefinition<CR>

    nnoremap <buffer> <localleader>os :<C-u>OmniSharpStartServer<CR>
    nnoremap <buffer> <localleader>oS :<C-u>OmniSharpStopServer<CR>
    nnoremap <buffer> <localleader>or :<C-u>OmniSharpRestartServer<CR>
    nnoremap <buffer> <localleader>oR :<C-u>OmniSharpRestartAllServers<CR>
endif

if vivid#enabled('omnisharp-vim')
    setlocal signcolumn=yes
    setlocal updatetime=500
    sign define OmniSharpCodeActions text=> texthl=Special
    autocmd CursorHold <buffer> call <SID>OmniSharpSignColumn()
endif

function! s:OmniSharpSignColumn() abort
    if OmniSharp#CountCodeActions({-> execute('sign unplace 99')})
        let l:line = getpos('.')[1]
        execute 'sign place 99 line='.l:line.' name=OmniSharpCodeActions buffer='.bufnr('%')
    endif
endfunction
