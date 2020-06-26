" =============================================================
" Description:  C# file type configuration.
" File:         ftplugin/cs.vim
" =============================================================

setlocal commentstring=//%s
setlocal completeopt-=preview completeopt+=popuphidden
setlocal completepopup+=border:off

" C# documentation (use OmniSharp instead of "&keywordprg")
nnoremap <buffer><silent> K :<C-u>OmniSharpDocumentation<CR>
nnoremap <buffer><silent> gd :<C-u>OmniSharpGotoDefinition<CR>
nnoremap <buffer><silent> <C-]> :<C-u>OmniSharpGotoDefinition<CR>
nnoremap <buffer> <F12> :<C-u>OmniSharpPreviewDefinition<CR>
nnoremap <buffer> <S-F12> :<C-u>OmniSharpPreviewImplementation<CR>
nnoremap <buffer> <F2> :<C-u>OmniSharpRename<CR>

nnoremap <buffer> <localleader>a :<C-u>OmniSharpGetCodeActions<CR>
nnoremap <buffer> <localleader>U :<C-u>OmniSharpFixUsings<CR>
nnoremap <buffer> <localleader>u :<C-u>OmniSharpFindUsages<CR>
nnoremap <buffer> <localleader>i :<C-u>OmniSharpFindImplementations<CR>
nnoremap <buffer> <localleader>t :<C-u>OmniSharpTypeLookup<CR>
nnoremap <buffer> <localleader>s :<C-u>OmniSharpSignatureHelp<CR>

function! s:OmniSharpCodeActions() abort
    if bufname('%') ==# '' || !OmniSharp#IsServerRunning() | return | endif
    let opts = {
             \   'CallbackCount': function('s:OmniSharpCodeActionsReturnCount'),
             \   'CallbackCleanup': {-> execute('sign unplace 99')}
             \ }
    call OmniSharp#actions#codeactions#Count(opts)
endfunction

function! s:OmniSharpCodeActionsReturnCount(count) abort
    if a:count
        let l = getpos('.')[1]
        let f = expand('%:p')
        execute 'sign place 99 line='.l.' name=OmniSharpCodeActions file='.f
    endif
endfunction

setlocal signcolumn=yes updatetime=500
sign define OmniSharpCodeActions text=> texthl=Special
autocmd! CursorHold <buffer> call <SID>OmniSharpCodeActions()
