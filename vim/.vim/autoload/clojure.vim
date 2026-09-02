vim9script

# TODO: translate to old Vim Script?

def Symbol(sym: string): string
    return sym -> axvr#Else(expand('<cword>')) -> substitute('\', '', 'g') -> trim()
enddef

def FixNs(ns: string): string
    return ns -> Symbol() -> substitute('\m/\k*$', '', '')
enddef

export def FormatNsAsPath(ns: string): string
    return tr(FixNs(ns), '-.', '_/')
enddef

def Quote(expr: string): string
    return (expr =~# "^'" ? expr : "'" .. expr)
enddef

def Keyword(expr: string): string
    return (expr =~# '^:' ? expr : ':' .. expr)
enddef

def String(expr: string): string
    return '"' .. expr .. '"'
enddef

def List(...exprs: list<string>): string
    return join(exprs, ' ')
enddef

def Apply(expr: string, func: string): string
    return '(' .. func .. ' ' .. expr .. ')'
enddef

export def Doc(sym: string)
    sym -> Symbol() -> Apply('clojure.repl/doc') -> zepl#send()
enddef

export def Source(sym: string)
    sym -> Symbol() -> Apply('clojure.repl/source') -> zepl#send()
enddef

export def Apropos(txt: string)
    txt -> Symbol()
        -> String()
        -> Apply('clojure.repl/apropos')
        -> Apply('clojure.pprint/pprint')
        -> zepl#send()
enddef

export def Dir(ns: string)
    ns -> FixNs() -> Apply('clojure.repl/dir') -> zepl#send()
enddef

export def FindDoc(txt: string)
    txt -> String()
        -> substitute('\', '\\\', 'g')
        -> Apply('clojure.repl/find-doc')
        -> zepl#send()
enddef

export def Require(ns: string, reload = false)
    ns -> FixNs()
       -> Quote()
       -> List((reload ? ' :reload' : ''))
       -> Apply('clojure.core/require')
       -> zepl#send()
enddef

export def Import(ns: string)
    ns -> FixNs() -> Quote() -> Apply('clojure.core/import') -> zepl#send()
enddef

export def Use(ns: string)
    ns -> FixNs() -> Quote() -> Apply('clojure.core/use') -> zepl#send()
enddef

export def NsUnmap(ns: string, sym: string)
    ns -> FixNs()
       -> List(sym -> Symbol() -> Quote())
       -> Apply('clojure.core/ns-unmap')
       -> zepl#send()
enddef

export def NsUnalias(ns: string, sym: string)
    ns -> FixNs()
       -> List(sym -> Symbol() -> Quote())
       -> Apply('clojure.core/ns-unalias')
       -> zepl#send()
enddef

export def GetNs(file = '%'): string
    const FindNs = (ln) => matchstr(ln, '\m(ns\s\+\zs\(\k\+\)\ze')
    if bufnr(file) == -1
        for line in readfile(file, '', 50)
            const ns = FindNs(line)
            if empty(ns) | return ns | endif
        endfor
    else
        var lnr = 1
        while lnr <= line('$')
            const line = getbufline(file, lnr)
            if !empty(line)
                const ns = FindNs(line[0])
                if !empty(ns) | return ns | endif
            endif
            lnr += 1
        endwhile
    endif
    return ''
enddef

export def ChangeNs(ns = '')
    var ns2 = ns

    if empty(trim(ns2))
        ns2 = GetNs('%')
    endif

    if empty(ns2)
        axvr#Err('No namespace specified.')
    else
        ns2 -> Apply('clojure.core/ns') -> zepl#send()
    endif
enddef

# CustomList function for :command-complete to complete Clojure syntax keywords.
export def CmdComplete(text: string, wholecmd: string, curpos: number): list<string>
    return axvr#MatchFuzzy(uniq(clojurecomplete#Complete(0, '')['words']), text)
enddef
