" Description:  Gerbil Scheme config.
" File:         ftplugin/gerbil.vim

let b:repl_config = { 'cmd': 'gxi', 'load_file': '(load "%s")' }
setl lispwords+=def
