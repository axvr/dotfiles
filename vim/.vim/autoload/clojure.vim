vim9script

def ErrorMsg(msg: string)
    echohl ErrorMsg
    echo msg
    echohl NONE
enddef

def FixSymbol(symbol: string): string
    return substitute(symbol, '\', '', 'g')
enddef

def FixNs(ns: string): string
    return ns -> substitute('\', '', 'g')
              -> substitute('\m/\k*$', '', '')
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

def Concat(...exprs: list<string>): string
    return join(exprs, ' ')
enddef

def Apply(expr: string, func: string): string
    return '(' .. func .. ' ' .. expr .. ')'
enddef

def PrettyPrint(expr: string): string
    return expr -> Apply('clojure.pprint/pprint')
enddef

export def Doc(sym: string)
    sym -> FixSymbol()
        -> Apply('clojure.repl/doc')
        -> zepl#send()
enddef

export def Source(sym: string)
    sym -> FixSymbol()
        -> Apply('clojure.repl/source')
        -> zepl#send()
enddef

export def Apropos(sym: string)
    sym -> FixSymbol()
        -> String()
        -> Apply('clojure.repl/apropos')
        -> PrettyPrint()
        -> zepl#send()
enddef

export def NsPublics(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('ns-publics')
       -> Apply('keys')
       -> PrettyPrint()
       -> zepl#send()
enddef

export def Require(ns: string, reload = false)
    ns -> FixNs()
       -> Quote()
       -> Concat((reload ? ' :reload' : ''))
       -> Apply('clojure.core/require')
       -> zepl#send()
enddef

export def Import(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('clojure.core/import')
       -> zepl#send()
enddef

export def Use(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('clojure.core/use')
       -> zepl#send()
enddef

export def NsUnmap(ns: string, sym: string)
    ns -> FixNs()
       -> Concat(sym -> FixSymbol() -> Quote())
       -> Apply('clojure.core/ns-unmap')
       -> zepl#send()
enddef

export def NsUnalias(ns: string, sym: string)
    ns -> FixNs()
       -> Concat(sym -> FixSymbol() -> Quote())
       -> Apply('clojure.core/ns-unalias')
       -> zepl#send()
enddef

export def Ns(file = '%'): string
    const GetNs = (ln) => matchstr(ln, '\m(ns\s\+\zs\(\k\+\)\ze')
    if bufnr(file) == -1
        for line in readfile(file, '', 100)
            const ns = GetNs(line)
            if empty(ns) | return ns | endif
        endfor
    else
        var lnr = 1
        while lnr <= line('$')
            const line = getbufline(file, lnr)
            if !empty(line)
                const ns = GetNs(line[0])
                if !empty(ns) | return ns | endif
            endif
            lnr += 1
        endwhile
    endif
    return ''
enddef

export def ChangeNs(ns: string)
    var ns2 = ns

    if empty(trim(ns2))
        ns2 = Ns('%')
    endif

    if empty(ns2)
        ErrorMsg('No namespace specified.')
    else
        ns2 -> Apply('clojure.core/ns') -> zepl#send()
    endif
enddef

# TODO
# def GetSymbolUnderCursor()
# enddef
