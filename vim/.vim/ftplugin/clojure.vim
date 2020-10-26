let b:repl_config = { 'cmd': 'clj', 'load_file': '(load-file "%s")' }

command! -buffer -bar -nargs=1 CljDoc :call zepl#send('(clojure.repl/doc ' . substitute(<q-args>, '\', '', 'g') . ')')
command! -buffer -bar -nargs=1 CljSrc :call zepl#send('(clojure.repl/source ' . <q-args> . ')')
command! -buffer -bar -nargs=+ CljNs  :call zepl#send('(clojure.core/ns ' . <q-args> . ')')

let &l:keywordprg = ":CljDoc"
