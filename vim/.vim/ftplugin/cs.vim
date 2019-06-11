" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

packadd omnisharp

setlocal commentstring=//%s
setlocal completeopt& completeopt-=preview

" C# documentation (use OmniSharp instead of '&keywordprg')
nnoremap <buffer><silent> K :<C-u>OmniSharpDocumentation<CR>
nnoremap <buffer><silent> <C-]> :<C-u>OmniSharpGotoDefinition<CR>
nnoremap <buffer> <F2> :<C-u>OmniSharpRename<CR>

nnoremap <buffer> <localleader>a :<C-u>OmniSharpGetCodeActions<CR>
nnoremap <buffer> <localleader>U :<C-u>OmniSharpFixUsings<CR>
nnoremap <buffer> <localleader>u :<C-u>OmniSharpFindUsages<CR>
nnoremap <buffer> <localleader>i :<C-u>OmniSharpFindImplementations<CR>
nnoremap <buffer> <localleader>t :<C-u>OmniSharpTypeLookup<CR>
nnoremap <buffer> <localleader>m :<C-u>OmniSharpFindMembers<CR>
nnoremap <buffer> <localleader>r :<C-u>OmniSharpRestartServer<CR>
nnoremap <buffer> <localleader>R :<C-u>OmniSharpRestartAllServers<CR>
nnoremap <buffer> <localleader>s :<C-u>OmniSharpStartServer<CR>
nnoremap <buffer> <localleader>S :<C-u>OmniSharpStopServer<CR>

function! s:OmniSharpSignColumn() abort
    if OmniSharp#CountCodeActions({-> execute('sign unplace 99')})
        exe 'sign place 99 line='.getpos('.')[1].' name=OmniSharpCodeActions buffer='.bufnr('%')
    endif
endfunction

setlocal signcolumn=yes updatetime=500
sign define OmniSharpCodeActions text=> texthl=Special
autocmd! CursorHold <buffer> call <SID>OmniSharpSignColumn()
