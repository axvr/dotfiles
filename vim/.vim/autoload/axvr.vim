" Collection of personal helper functions for Vim.

function! axvr#Warn(msg)
    echohl WarningMsg | echomsg a:msg | echohl NONE
endfunction

function! axvr#Err(msg)
    echohl ErrorMsg | echomsg a:msg | echohl NONE
endfunction

function! axvr#YN(qn) abort
    return confirm(a:qn, "&Yes\n&No", 0, 'Question') == 1
endfunction

function! axvr#FuzzyMatch(list, search) abort
    return empty(a:search) ? a:list : matchfuzzy(a:list, a:search)
endfunction

function! axvr#TempSetBufOpt(opt, val, callback)
    let buf = bufnr('%')
    let prevval = getbufvar(buf, a:opt)
    call setbufvar(buf, a:opt, a:val)
    call a:callback()
    call setbufvar(buf, a:opt, prevval)
endfunction

function! axvr#TempGrep(prg, args)
    call axvr#TempSetBufOpt('&grepprg', a:prg, {-> execute('grep ' . a:args)})
endfunction

let s:trust_store = expand($HOME . '/.vim/state/trust/')
call mkdir(s:trust_store, 'p')

function! axvr#IsTrusted(uid) abort
    return filereadable(s:trust_store . a:uid)
endfunction

function! axvr#Trust(uid) abort
    return writefile([], s:trust_store . a:uid)
endfunction

function! axvr#Untrust(uid) abort
    call delete(s:trust_store . a:uid, 'rf')
endfunction
