setlocal nojoinspaces

let b:is_chicken = 1

let b:repl_config = {
    \   'cmd': 'rlwrap -r -q "\"" -b "(){}[],^%#@\";:" csi -:c',
    \   'load_file': '(load "%s")'
    \ }
