let g:clojure_discard_macro = 1

compiler clj-kondo
setlocal nojoinspaces
setlocal path=,,src/**,test/**,resources/**,dev/**
setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.cljr,.edn,.bb,.clj_kondo
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

nmap <buffer> <C-]> <LocalLeader>gd
