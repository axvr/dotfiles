vim9script

b:repl_config = { 'cmd': 'sbcl-repl', 'load_file': '(load "%s")' }

setlocal lispwords+=loop
setlocal iskeyword+=&
setlocal keywordprg=:Describe

command! -buffer -bar -nargs=1 Describe :call lisp#Describe(<q-args>)
command! -buffer -bar -nargs=1 InPackage :call lisp#InPackage(<q-args>)
command! -buffer -bar -nargs=1 Quickload :call lisp#Quickload(<q-args>)
command! -buffer -bar -nargs=1 Introspect :call lisp#Introspect(<q-args>)
command! -buffer -bar -nargs=1 Disassemble :call lisp#Disassemble(<q-args>)
command! -buffer -bang -bar -nargs=+ Apropos :call Apropos(<q-args>, <q-bang> == '!')

def Apropos(sym_and_pkg: string, extern: bool)
    const s = split(trim(sym_and_pkg), '\s\+', 0)
    const l = len(s)
    if l == 1
        call lisp#Apropos(s[0], '', extern)
    elseif l == 2
        call lisp#Apropos(s[0], s[1], extern)
    endif
enddef

# Use function keys to control the debugger.
nnoremap <silent> <F1> :<C-u>call zepl#send('1')<CR>
nnoremap <silent> <F2> :<C-u>call zepl#send('2')<CR>
nnoremap <silent> <F3> :<C-u>call zepl#send('3')<CR>
nnoremap <silent> <F4> :<C-u>call zepl#send('4')<CR>
nnoremap <silent> <F5> :<C-u>call zepl#send('5')<CR>
nnoremap <silent> <F6> :<C-u>call zepl#send('6')<CR>
nnoremap <silent> <F7> :<C-u>call zepl#send('7')<CR>
nnoremap <silent> <F8> :<C-u>call zepl#send('8')<CR>
nnoremap <silent> <F9> :<C-u>call zepl#send('9')<CR>
nnoremap <silent> <F10> :<C-u>call zepl#send('0')<CR>
nnoremap <silent> <F11> :<C-u>call zepl#send('0')<CR>
nnoremap <silent> <F12> :<C-u>call zepl#send('0')<CR>
