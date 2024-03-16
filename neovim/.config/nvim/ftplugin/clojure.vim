let g:clojure_discard_macro = 1

setlocal nojoinspaces

compiler clj-kondo

setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.edn,.bb
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

nmap <C-]> <LocalLeader>gd

if filereadable('tests.edn')
    let g:conjure#client#clojure#nrepl#test#runner = 'kaocha'
endif
