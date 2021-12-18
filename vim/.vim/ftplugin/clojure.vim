let b:repl_config = { 'cmd': 'clj-socket', 'load_file': '(load-file "%s")' }

let g:clojure_discard_macro = 1

command! -buffer -bar -nargs=* Ns        :call s:change_ns(<q-args>)

command! -buffer -bar -nargs=1 Doc       :call zepl#send('(clojure.repl/doc ' . substitute(<q-args>, '\', '', 'g') . ')')
command! -buffer -bar -nargs=1 Source    :call zepl#send('(clojure.repl/source ' . <q-args> . ')')
command! -buffer -bar -nargs=1 Apropos   :call zepl#send('(clojure.pprint/pprint (clojure.repl/apropos "' . <q-args> . '"))')
command! -buffer -bar -nargs=1 NsPublics :call zepl#send("(clojure.pprint/pprint (keys (ns-publics '" . <q-args> . ')))')

command! -buffer -bar -nargs=1 Require   :call zepl#send("(clojure.core/require '" . <q-args> . ' :reload)')
command! -buffer -bar -nargs=1 Import    :call zepl#send("(clojure.core/import '" . <q-args> . ')')
command! -buffer -bar -nargs=1 Use       :call zepl#send("(clojure.core/use '" . <q-args> . ')')

command! -buffer -bar -nargs=1 NsUnmap   :call zepl#send("(clojure.core/ns-unmap *ns* '" . <q-args> . ")")
command! -buffer -bar -nargs=1 NsUnalias :call zepl#send("(clojure.core/ns-unalias *ns* '" . <q-args> . ")")

let &l:keywordprg = ':Doc'

let &l:makeprg = 'clj -M:lint'
setlocal errorformat=%f:%l:%c:\ %trror:\ %m,
                    \%f:%l:%c:\ %tarning:\ %m,
                    \%-G%m

function! s:change_ns(ns)
    let ns = a:ns

    if empty(trim(ns))
        let lines = getbufline('%', 1, '$')
        for l in lines
            let ns = matchstr(l, '\m\C(ns\s\+\zs\(\k\+\)\ze')
            if !empty(ns) | break | endif
        endfor
    endif

    if empty(ns)
        echohl ErrorMsg
        echo 'No namespace specified.'
        echohl NONE
    else
        call zepl#send('(clojure.core/ns ' . ns . ')')
    endif
endfunction

nnoremap gzn :<C-u>Ns<CR>

" TODO: dedicated :Lint and :Test commands.
" TODO: make :Source default to symbol under cursor.
