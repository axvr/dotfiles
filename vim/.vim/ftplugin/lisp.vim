let b:omni_syntax_ignorecase = 1
setlocal nojoinspaces
setlocal iskeyword+=&,:
setlocal keywordprg=:LispDescribe

" TODO: switch to ECL?  Can it pick automatically?
let b:repl_config = { 'cmd': 'sbcl-repl', 'load_file': '(load "%s")' }

com! -buffer -bar -nargs=1 LispInPackage  call lisp#InPackage(<q-args>)
com! -buffer -bar -nargs=1 LispQuickload  call lisp#Quickload(<q-args>)
com! -buffer -bar -nargs=1 LispIntrospect call lisp#Introspect(s:TryUseCurSym(<q-args>))
com! -buffer -bar -nargs=+ LispUnintern   call lisp#Unintern(<f-args>)
com! -buffer -bar -nargs=? LispMakunbound call lisp#Makunbound(s:TryUseCurSym(<q-args>))

com! -buffer       -bar -nargs=? -complete=customlist,axvr#CmdComplete LispDescribe      call lisp#Describe(s:TryUseCurSym(<q-args>))
com! -buffer       -bar -nargs=+ -complete=customlist,axvr#CmdComplete LispDocumentation call lisp#Documentation(<f-args>)
com! -buffer       -bar -nargs=? -complete=customlist,axvr#CmdComplete LispDisassemble   call lisp#Disassemble(s:TryUseCurSym(<q-args>))
com! -buffer -bang -bar -nargs=* -complete=customlist,axvr#CmdComplete LispApropos       call s:Apropos(s:TryUseCurSym(<q-args>), <q-bang> == '!')
com! -buffer       -bar -nargs=? -complete=customlist,axvr#CmdComplete LispHyperSpec     call lisp#HyperSpec(s:TryUseCurSym(<q-args>))

function! s:TryUseCurSym(override) abort
    return axvr#Else(a:override, lisp#GetSymbol())
endfunction

function! s:Apropos(sym_and_pkg, extern) abort
    let s = split(trim(a:sym_and_pkg), '\s\+', 0)
    call lisp#Apropos(s[0], get(s, 1, ''), a:extern)
endfunction

" TODO: `gzn` binding like in ftplugin/clojure.vim to switch to package the
" current file is in.

nnoremap <silent> <C-K> :<C-u>LispHyperSpec<CR>

" Select a debugger option.
nnoremap <silent> gzd :<C-u>call zepl#send(getcharstr())<CR>

" Exit debugger and/or REPL.
nnoremap <silent> gz<C-d> :<C-u>call zepl#send("<C-d>", 1)<CR>
