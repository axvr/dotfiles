" Description:  Ascribe extension to override default filetype detection.
" File:         after/plugin/ascribe_ftdetect.vim

function! s:filetype(value) dict
    if empty(a:value)
        echoerr 'Ascribe: No "filetype" specified.'
    else
        execute 'silent! setlocal filetype=' . a:value
    endif
endfunction

if exists('g:ascribe_handlers')
    let g:ascribe_handlers['vim-filetype'] = function('<SID>filetype')
    " let g:ascribe_handlers['linguist-language'] = function('<SID>filetype')
endif
