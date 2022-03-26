vim9script

def FixSymbol(symbol: string): string
    return substitute(trim(symbol), "[:\"'\\`,]", '', 'g')
enddef

def ErrorMsg(msg: string)
    echohl ErrorMsg
    echo msg
    echohl NONE
enddef

def Quote(expr: string): string
    return "'" .. expr
enddef

def String(expr: string): string
    return '"' .. expr .. '"'
enddef

def Keyword(expr: string): string
    return ':' .. expr
enddef

def Function(expr: string): string
    return "#'" .. expr
enddef

def Concat(...exprs: list<string>): string
    return exprs -> map((k, v) => trim(v))
                 -> filter((k, v) => v !=# '')
                 -> join(' ')
enddef

def Apply(expr: string, func: string): string
    return '(' .. func .. ' ' .. expr .. ')'
enddef

export def Describe(sym: string)
    sym -> FixSymbol()
        -> Apply('describe')
        -> zepl#send()
enddef

export def InPackage(pkg: string)
    pkg -> FixSymbol()
        -> Keyword()
        -> Apply('in-package')
        -> zepl#send()
enddef

export def Apropos(sym: string, pkg = '', extern: bool = false)
    const ex = (extern ? 't' : '')
    const pk = (pkg == ''
                ? (extern ? '*package*' : '')
                : (pkg -> FixSymbol() -> Keyword()))
    sym -> FixSymbol()
        -> String()
        -> Concat(pk, ex)
        -> Apply('apropos')
        -> zepl#send()
enddef

export def Introspect(sym: string)
    sym -> FixSymbol()
        -> Function()
        -> Apply('sb-introspect:find-definition-source')
        -> zepl#send()
enddef

export def Disassemble(sym: string)
    sym -> FixSymbol()
        -> Quote()
        -> Apply('disassemble')
        -> zepl#send()
enddef

export def Quickload(pkg: string)
    pkg -> FixSymbol()
        -> Keyword()
        -> Apply('ql:quickload')
        -> zepl#send()
enddef
