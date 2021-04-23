let b:is_chicken = 1

if b:is_chicken
    let b:repl_config = {
                \   'cmd': 'rlwrap -r -q "\"" -b "(){}[],^%#@\";:" csi -:c',
                \   'load_file': '(load "%s")'
                \ }

    setlocal lispwords+=with-input-from-string,with-output-to-string
    setlocal lispwords+=call-with-output-file,call/cc,rec
endif
