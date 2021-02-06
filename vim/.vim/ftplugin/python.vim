runtime zepl/contrib/python.vim

let b:repl_config = {
            \   'cmd': 'python',
            \   'formatter': function("zepl#contrib#python#formatter")
            \ }
