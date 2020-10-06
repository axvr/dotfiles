" =============================================================
" Description:  Use Ascribe to configure C formatting style.
" File:         ~/.vim/after/ftplugin/c.vim
" Ascribe:      <https://axvr.io/projects/ascribe/>
" =============================================================

function! <SID>cstyle(style) dict
    if a:style ==# 'gnu'
        call cstyle#gnu()
    elseif a:style ==# 'vim'
        call cstyle#vim()
    else
        echohl ErrorMsg
        echom "C style '" . a:style . "' is not supported."
        echohl None
    endif
endfunction

let b:ascribe_handlers = { 'cstyle': function('s:cstyle') }
