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

function! axvr#MatchFuzzy(list, search, opts = {}) abort
    return empty(a:search) ? a:list : matchfuzzy(a:list, a:search, a:opts)
endfunction

function! axvr#TempSetBufOpt(opt, val, callback)
    let buf = bufnr('%')
    let prevval = getbufvar(buf, a:opt)
    call setbufvar(buf, a:opt, a:val)
    call a:callback()
    call setbufvar(buf, a:opt, prevval)
endfunction

function! axvr#GrepWith(prg, args = '', opts = {})
    let jump = get(a:opts, 'jump', 1) ? ''    : '!'
    let add  = get(a:opts, 'add', 0)  ? 'add' : ''
    let loc  = get(a:opts, 'loc', 0)  ? 'l'   : ''
    let grep =  loc..'grep'..add..jump..' '
    call axvr#TempSetBufOpt('&grepprg', a:prg, {-> execute(grep . a:args, 'silent')})
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
