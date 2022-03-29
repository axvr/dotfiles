vim9script

def FixSymbol(symbol: string): string
    return substitute(trim(symbol), "[\"'\\`,]", '', 'g')
enddef

def Quote(expr: string): string
    return (expr =~# "^'" ? expr : "'" .. expr)
enddef

def String(expr: string): string
    return '"' .. expr .. '"'
enddef

def Keyword(expr: string): string
    return (expr =~# '^:' ? expr : ':' .. expr)
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
        -> Quote()
        -> Apply('describe')
        -> zepl#send()
enddef

export def Documentation(sym: string, obj_type: string)
    sym -> FixSymbol()
        -> Quote()
        -> Concat(obj_type -> FixSymbol() -> Quote())
        -> Apply('documentation')
        -> zepl#send()
enddef

# TODO: if no package is given, switch to package the current file is in.
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

export def Unintern(sym: string, pkg = '')
    sym -> FixSymbol()
        -> Quote()
        -> Concat((pkg == '' ? '' : pkg -> FixSymbol() -> Keyword()))
        -> Apply('unintern')
        -> zepl#send()
enddef

export def Makunbound(sym: string)
    sym -> FixSymbol()
        -> Quote()
        -> Apply('makunbound')
        -> zepl#send()
enddef

export def Quickload(pkg: string)
    pkg -> FixSymbol()
        -> Keyword()
        -> Apply('ql:quickload')
        -> zepl#send()
enddef
