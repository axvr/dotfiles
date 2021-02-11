" Description:  Use Ascribe to configure C formatting style.
" File:         after/ftplugin/c.vim
" Ascribe:      <https://axvr.io/projects/ascribe/>

function! s:cstyle(style) dict
    if a:style ==# 'gnu'
        call cstyle#gnu()
    elseif a:style ==# 'vim'
        call cstyle#vim()
    else
        echoerr "C style '" . a:style . "' is not supported."
    endif
endfunction

let b:ascribe_handlers = { 'cstyle': function('s:cstyle') }
