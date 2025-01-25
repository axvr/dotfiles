let &l:isfname = &l:iskeyword

if ! exists('g:conjure#client#clojure#nrepl#connection#auto_repl#cmd')
    if filereadable('deps.edn')
        let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'clojure -M:repl/nrepl'
    elseif filereadable('project.clj')
        let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'lein repl'
    elseif filereadable('bb.edn')
        " Use default.
    endif
endif

if ! exists('g:conjure#client#clojure#nrepl#test#runner') && filereadable('tests.edn')
    let g:conjure#client#clojure#nrepl#test#runner = 'kaocha'
endif
