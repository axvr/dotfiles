" Collection of personal helper functions for Vim.

function! axvr#Info(msg)
    echo a:msg
endfunction

function! axvr#Warn(msg)
    try
        echohl WarningMsg | echomsg a:msg
    finally
        echohl NONE
    endtry
endfunction

function! axvr#Err(msg)
    try
        echohl ErrorMsg | echomsg a:msg
    finally
        echohl NONE
    endtry
endfunction

function! axvr#YN(qn) abort
    return confirm(a:qn, "&Yes\n&No", 0, 'Question') == 1
endfunction

function! axvr#Ask(prompt, default = '', completion = 'file') abort
    try
        echohl Question
        call inputsave()
        let resp = input(a:prompt, a:default, a:completion)
        call inputrestore()
        return resp
    finally
        echohl NONE
    endtry
endfunction

function! axvr#AskCreateDirs(dirs) abort
    if ! isdirectory(a:dirs) && axvr#YN('Create directory?')
        call mkdir(a:dirs, 'p')
    endif
endfunction

function! axvr#ReEscape(str) abort
    return escape(a:str, './\^$~*')
endfunction

function! axvr#Else(str_or_col, fallback)
    return empty(a:str_or_col) ? a:fallback : a:str_or_col
endfunction

function! axvr#Conf(name, default) abort
    return get(b:, a:name, get(g:, a:name, a:default))
endfunction

function! axvr#MatchFuzzy(list, search, opts = {}) abort
    return empty(a:search) ? a:list : matchfuzzy(a:list, a:search, a:opts)
endfunction

function! axvr#TempSetBufOpt(opt, val, callback)
    let buf = bufnr('%')
    let prevval = getbufvar(buf, a:opt)
    try
        call setbufvar(buf, a:opt, a:val)
        call a:callback()
    finally
        call setbufvar(buf, a:opt, prevval)
    endtry
endfunction

" CustomList function for `:command-complete` to complete syntax keywords.
function! axvr#CmdComplete(text, wholecmd, curpos) abort
    return axvr#MatchFuzzy(uniq(syntaxcomplete#Complete(0, '')), a:text)
endfunction

let s:trust_store = expand($HOME .. '/.vim/state/trust/')
call mkdir(s:trust_store, 'p')

function! axvr#IsTrusted(uid) abort
    return filereadable(s:trust_store .. a:uid)
endfunction

function! axvr#Trust(uid) abort
    return writefile([], s:trust_store .. a:uid)
endfunction

function! axvr#Untrust(uid) abort
    call delete(s:trust_store .. a:uid, 'rf')
endfunction
