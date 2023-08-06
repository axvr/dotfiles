let g:clojure_discard_macro = 1

compiler clj-kondo

setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.edn,.bb
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

nmap <C-]> <LocalLeader>gd
