func! s:Symbol(sym) abort
    return a:sym->substitute('\', '', 'g')->trim()
endfunc

func! clojure#GetSymbol() abort
    return s:Symbol(expand('<cword>'))
endfunc

func! s:FixNs(ns) abort
    return a:ns->s:Symbol()->substitute('\m/\k*$', '', '')
endfunc

func! clojure#FormatNsAsPath(ns) abort
    return tr(s:FixNs(a:ns), '-.', '_/')
endfunc

func! s:Quote(expr) abort
    return a:expr =~# "^'" ? a:expr : "'" .. a:expr
endfunc

func! s:Keyword(expr) abort
    return a:expr =~# '^:' ? a:expr : ':' .. a:expr
endfunc

func! s:String(expr) abort
    return '"' .. a:expr .. '"'
endfunc

func! s:List(...) abort
    return join(a:000, ' ')
endfunc

func! s:Apply(expr, func) abort
    return '(' .. a:func .. ' ' .. a:expr .. ')'
endfunc

func! clojure#Doc(sym) abort
    return a:sym->s:Symbol()->s:Apply('clojure.repl/doc')->zepl#send()
endfunc

func! clojure#Source(sym) abort
    return a:sym->s:Symbol()->s:Apply('clojure.repl/source')->zepl#send()
endfunc

func! clojure#Apropos(txt) abort
    return a:txt->s:Symbol()
         \ ->s:String()
         \ ->s:Apply('clojure.repl/apropos')
         \ ->s:Apply('clojure.pprint/pprint')
         \ ->zepl#send()
endfunc

func! clojure#Dir(ns) abort
    return a:ns->s:FixNs()->s:Apply('clojure.repl/dir')->zepl#send()
endfunc

func! clojure#FindDoc(txt) abort
    return a:txt->s:String()
         \ ->substitute('\', '\\\', 'g')
         \ ->s:Apply('clojure.repl/find-doc')
         \ ->zepl#send()
endfunc

func! clojure#Require(ns, reload = false) abort
    return a:ns->s:FixNs()->s:Quote()
         \ ->s:List((a:reload ? ' :reload' : ''))
         \ ->s:Apply('clojure.core/require')
         \ ->zepl#send()
endfunc

func! clojure#Import(ns) abort
    return a:ns->s:FixNs()->s:Quote()->s:Apply('clojure.core/import')->zepl#send()
endfunc

func! clojure#Use(ns) abort
    return a:ns->s:FixNs()->s:Quote()->s:Apply('clojure.core/use')->zepl#send()
endfunc

func! clojure#NsUnmap(ns, sym) abort
    let ns = (a:ns ==# '*ns*' ? a:ns : s:Quote(a:ns))

    return ns
         \ ->s:List(a:sym->s:Symbol()->s:Quote())
         \ ->s:Apply('clojure.core/ns-unmap')
         \ ->zepl#send()
endfunc

func! clojure#NsUnalias(ns, sym) abort
    let ns = (a:ns ==# '*ns*' ? a:ns : Quote(a:ns))

    return ns
         \ ->s:List(a:sym->s:Symbol()->s:Quote())
         \ ->s:Apply('clojure.core/ns-unalias')
         \ ->zepl#send()
endfunc

func! clojure#GetNs(file = '%') abort
    let ns_re = '\m(ns\s\+\zs\(\k\+\)\ze'
    if bufnr(a:file) == -1
        for line in readfile(a:file, '', 50)
            const ns = matchstr(line, ns_re)
            if empty(ns) | return ns | endif
        endfor
    else
        let lnr = 1
        while lnr <= line('$')
            let line = getbufline(a:file, lnr)
            if !empty(line)
                let ns = matchstr(line[0], ns_re)
                if !empty(ns) | return ns | endif
            endif
            let lnr += 1
        endwhile
    endif
    return ''
endfunc

func! clojure#ChangeNs(ns = '') abort
    if empty(a:ns)
        call axvr#Err('No namespace specified.')
    else
        return a:ns->s:Apply('clojure.core/ns')->zepl#send()
    endif
endfunc

" CustomList func for :command-complete to complete Clojure syntax keywords.
func! clojure#CmdComplete(text, wholecmd, curpos) abort
    return axvr#MatchFuzzy(uniq(clojurecomplete#Complete(0, '')['words']), a:text)
endfunc
