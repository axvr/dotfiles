let b:repl_config = { 'cmd': 'clj', 'load_file': '(load-file "%s")' }

" let g:clojure_align_multiline_strings = 1

command! -buffer -bar -nargs=1 CljDoc :call zepl#send('(clojure.repl/doc ' . substitute(<q-args>, '\', '', 'g') . ')')
command! -buffer -bar -nargs=1 CljSrc :call zepl#send('(clojure.repl/source ' . <q-args> . ')')
command! -buffer -bar -nargs=+ CljNs  :call zepl#send('(clojure.core/ns ' . <q-args> . ')')
command! -buffer -bar -nargs=1 CljReq :call zepl#send("(clojure.core/require '" . <q-args> . ' :reload)')

let &l:keywordprg = ':CljDoc'
setlocal errorformat=%f:%l:%c:\ %trror:\ %m,
                    \%f:%l:%c:\ %tarning:\ %m
let &l:makeprg = 'clj -M:lint --lint src'
