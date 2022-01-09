vim9script

def FixSymbol(symbol: string): string
    return substitute(symbol, '\', '', 'g')
enddef

def FixNs(ns: string): string
    return ns -> substitute('\', '', 'g')
              -> substitute('\m/\k*$', '', '')
enddef

def ErrorMsg(msg: string)
    echohl ErrorMsg
    echo msg
    echohl NONE
enddef

export def clojure#format_ns_as_file_path(ns: string)
    return tr(FixNs(ns), '-.', '_/')
enddef

def Quote(expr: string): string
    return "'" .. expr
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

def PrettyPrint(expr: string)
    return expr->Apply('clojure.pprint/pprint')
enddef

export def clojure#doc(sym: string)
    sym -> FixSymbol()
        -> Apply('clojure.repl/doc')
        -> zepl#send()
enddef

export def clojure#source(sym: string)
    sym -> FixSymbol()
        -> Apply('clojure.repl/source')
        -> zepl#send()
enddef

export def clojure#apropos(sym: string)
    sym -> FixSymbol()
        -> String()
        -> Apply('clojure.repl/apropos')
        -> PrettyPrint()
        -> zepl#send()
enddef

export def clojure#ns_publics(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('ns-publics')
       -> Apply('keys')
       -> PrettyPrint()
       -> zepl#send()
enddef

export def clojure#require(ns: string, reload = false)
    ns -> FixNs()
       -> Quote()
       -> Concat((reload ? ' :reload' : ''))
       -> Apply('clojure.core/require')
       -> zepl#send()
enddef

export def clojure#import(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('clojure.core/import')
       -> zepl#send()
enddef

export def clojure#use(ns: string)
    ns -> FixNs()
       -> Quote()
       -> Apply('clojure.core/use')
       -> zepl#send()
enddef

export def clojure#ns_unmap(ns: string, sym: string)
    ns -> FixNs()
       -> Concat(sym -> FixSymbol() -> Quote())
       -> Apply('clojure.core/ns-map')
       -> zepl#send()
enddef

export def clojure#ns_unalias(ns: string, sym: string)
    ns -> FixNs()
       -> Concat(sym -> FixSymbol() -> Quote())
       -> Apply('clojure.core/ns-unalias')
       -> zepl#send()
enddef

export def clojure#ns(file = '%'): string
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

export def clojure#change_ns(ns: string)
    var ns2 = ns

    if empty(trim(ns2))
        ns2 = clojure#ns('%')
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
