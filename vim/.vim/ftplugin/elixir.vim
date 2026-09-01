setlocal signcolumn=yes
compiler mix

let b:repl_config = {
\   'cmd': filereadable('mix.exs') ? 'iex -S mix' : 'iex',
\   'load_file': 'c("%s")'
\ }
