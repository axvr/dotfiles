func! s:FixSymbol(sym) abort
    return substitute(trim(a:sym), "[\"'\\`,]", '', 'g')
endfunc

func! lisp#GetSymbol() abort
    return s:FixSymbol(expand('<cword>'))
endfunc

func! s:Quote(expr) abort
    return (a:expr =~# "^'" ? a:expr : "'" .. a:expr)
endfunc

func! s:String(expr) abort
    return '"' .. a:expr .. '"'
endfunc

func! s:Keyword(expr) abort
    return a:expr =~# '^:' ? a:expr : ':' .. a:expr
endfunc

func! s:Function(expr) abort
    return "#'" .. a:expr
endfunc

func! s:List(...) abort
    return a:000->copy()
         \ ->map('trim(v:val)')
         \ ->filter('!empty(v:val)')
         \ ->join(' ')
endfunc

func! s:Apply(expr, func) abort
    return '(' .. a:func .. ' ' .. a:expr .. ')'
endfunc

func! lisp#Describe(sym) abort
    return a:sym->s:FixSymbol()->s:Quote()->s:Apply('describe')->zepl#send()
endfunc

func! lisp#Documentation(sym, obj_type) abort
    return a:sym->s:FixSymbol()->s:Quote()
         \ ->s:List(a:obj_type->s:FixSymbol()->s:Quote())
         \ ->s:Apply('documentation')
         \ ->zepl#send()
endfunc

" TODO: if no package is given, switch to package the current file is in.
func! lisp#InPackage(pkg) abort
    return a:pkg->s:FixSymbol()->s:Keyword()->s:Apply('in-package')->zepl#send()
endfunc

func! lisp#Apropos(sym, pkg = '', extern = 0) abort
    let ex = a:extern ? 't' : ''
    let pk = a:pkg == ''
                \ ? (a:extern ? '*package*' : '')
                \ : (a:pkg->s:FixSymbol()->s:Keyword())

    return a:sym->s:FixSymbol()->s:String()
         \ ->s:List(pk, ex)
         \ ->s:Apply('apropos')
         \ ->zepl#send()
endfunc

func! lisp#Introspect(sym) abort
    return a:sym->s:FixSymbol()->s:Function()
         \ ->s:Apply('sb-introspect:find-definition-source')
         \ ->zepl#send()
endfunc

func! lisp#Disassemble(sym) abort
    return a:sym->s:FixSymbol()->s:Quote()
         \ ->s:Apply('disassemble')
         \ ->zepl#send()
endfunc

func! lisp#Unintern(sym, pkg = '') abort
    return a:sym->s:FixSymbol()->s:Quote()
         \ ->s:List((a:pkg == '' ? '' : a:pkg->s:FixSymbol()->s:Keyword()))
         \ ->s:Apply('unintern')
         \ -> zepl#send()
endfunc

func! lisp#Makunbound(sym) abort
    return a:sym->s:FixSymbol()->s:Quote()
         \ ->s:Apply('makunbound')
         \ ->zepl#send()
endfunc

func! lisp#Quickload(pkg) abort
    return a:pkg->s:FixSymbol()->s:Keyword()
         \ ->s:Apply('ql:quickload')
         \ ->zepl#send()
endfunc

func! lisp#HyperSpec(sym = '') abort
    call system('hyperspec ' .. shellescape(tolower(a:sym)))
endfunc
