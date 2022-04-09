vim9script

b:repl_config = { 'cmd': 'sbcl-repl', 'load_file': '(load "%s")' }
b:omni_syntax_ignorecase = 1

setlocal iskeyword+=&,:
setlocal keywordprg=:Describe

# TODO: gzn binding like in ftplugin/clojure.vim to switch to package the
# current file is in.

command! -buffer -bar -nargs=1 -complete=customlist,syntax#CmdComplete Describe
            \ :call lisp#Describe(<q-args>)
command! -buffer -bar -nargs=+ -complete=customlist,syntax#CmdComplete Documentation
            \ :call lisp#Documentation(<f-args>)
command! -buffer -bar -nargs=1 InPackage :call lisp#InPackage(<q-args>)
command! -buffer -bar -nargs=1 Quickload :call lisp#Quickload(<q-args>)
command! -buffer -bar -nargs=1 Introspect :call lisp#Introspect(<q-args>)
command! -buffer -bar -nargs=1 -complete=customlist,syntax#CmdComplete Disassemble
            \ :call lisp#Disassemble(<q-args>)
command! -buffer -bar -nargs=+ Unintern :call lisp#Unintern(<f-args>)
command! -buffer -bar -nargs=1 Makunbound :call lisp#Makunbound(<q-args>)
command! -buffer -bang -bar -nargs=+ -complete=customlist,syntax#CmdComplete Apropos
            \ :call Apropos(<q-args>, <q-bang> == '!')

command! -buffer -nargs=? -complete=customlist,syntax#CmdComplete HyperSpec
            \ :call lisp#HyperSpec(<q-args>)

def Apropos(sym_and_pkg: string, extern: bool)
    const s = split(trim(sym_and_pkg), '\s\+', 0)
    const l = len(s)
    if l == 1
        call lisp#Apropos(s[0], '', extern)
    elseif l == 2
        call lisp#Apropos(s[0], s[1], extern)
    endif
enddef

nnoremap <silent> <F1> :<C-u>call lisp#HyperSpec()<CR>

# Select a debugger option.
nnoremap <silent> gzd :<C-u>call zepl#send(getcharstr())<CR>

# Exit debugger and/or REPL.
nnoremap <silent> gz<C-d> :<C-u>call zepl#send("<C-d>", 1)<CR>
