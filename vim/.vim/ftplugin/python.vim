runtime zepl/contrib/python.vim

let b:repl_config = {
            \   'cmd': 'python',
            \   'formatter': function("zepl#contrib#python#formatter"),
            \   'load_file': 'exec(open("%s").read())'
            \ }

command! -buffer -bar -nargs=1 Help :call zepl#send('help(' . substitute(<q-args>, '\', '', 'g') . ')')
