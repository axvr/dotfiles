" =============================================================
" Description:  C# file type configuration.
" File:         ftplugin/cs.vim
" =============================================================

augroup csharp
    autocmd!
augroup END

setlocal commentstring=//%s
setlocal completeopt-=preview completeopt+=popuphidden
setlocal completepopup+=border:off
setlocal shortmess+=T

nmap <silent> <buffer> [[ <Plug>(omnisharp_navigate_up)
nmap <silent> <buffer> ]] <Plug>(omnisharp_navigate_down)

nmap <silent> <buffer> K <Plug>(omnisharp_documentation)

nmap <silent> <buffer> gd <Plug>(omnisharp_go_to_definition)
nmap <silent> <buffer> <C-]> <Plug>(omnisharp_go_to_definition)

nmap <silent> <buffer> <F2> <Plug>(omnisharp_rename)

nmap <silent> <buffer> <localleader>a <Plug>(omnisharp_code_actions)
xmap <silent> <buffer> <localleader>a <Plug>(omnisharp_code_actions)

nmap <silent> <buffer> <localleader>U <Plug>(omnisharp_fix_usings)
nmap <silent> <buffer> <localleader>u <Plug>(omnisharp_find_usages)
nmap <silent> <buffer> <localleader>i <Plug>(omnisharp_find_implementations)
nmap <silent> <buffer> <localleader>I <Plug>(omnisharp_preview_implementations)
nmap <silent> <buffer> <localleader>d <Plug>(omnisharp_go_to_definition)
nmap <silent> <buffer> <localleader>D <Plug>(omnisharp_preview_definition)

nmap <silent> <buffer> <C-\> <Plug>(omnisharp_signature_help)
imap <silent> <buffer> <C-\> <Plug>(omnisharp_signature_help)

nmap <silent> <buffer> gq <Plug>(omnisharp_code_format)

autocmd csharp CursorHold *.cs OmniSharpTypeLookup

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
autocmd csharp CursorHold <buffer> call <SID>OmniSharpCodeActions()
