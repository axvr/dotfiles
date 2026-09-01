let g:clojure_discard_macro = 1

if isdirectory('.clj-kondo')
    compiler clj-kondo
endif

setlocal nojoinspaces
setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.cljr,.edn,.bb,.clj_kondo
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

if has('nvim')
    nmap <buffer> <C-]> <LocalLeader>gd
endif

let b:repl_config = {
\   'cmd': filereadable('project.clj') ? 'lein repl' : 'clj-socket',
\   'load_file': '(clojure.core/load-file "%s")'
\ }
