let g:clojure_discard_macro = 1

compiler clj-kondo

setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.edn
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)
