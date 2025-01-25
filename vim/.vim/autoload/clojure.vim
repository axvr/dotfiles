function! clojure#FixNs(ns) abort
    return a:ns->substitute('\', '', 'g')->substitute('\m/\k*$', '', '')
endfunction

function! clojure#FormatNsAsPath(ns) abort
    return tr(clojure#FixNs(a:ns), '-.', '_/')
endfunction
