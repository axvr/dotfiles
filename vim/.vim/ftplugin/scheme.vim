" =============================================================
" Description:  Scheme configuration for Vim
" File:         ftplugin/scheme.vim
" =============================================================

highlight! def link schemeParentheses Delimiter

let b:is_chicken = get(b:, 'is_chicken', get(g:, 'is_chicken', 1))

if b:is_chicken
    let b:repl_config = {
                \   'cmd': 'rlwrap -r -q "\"" -b "(){}[],^%#@\";:" csi -:c',
                \   'load_file': '(load "%s")'
                \ }

    " https://wiki.call-cc.org/vim
    setl lispwords+=let-values,condition-case,with-input-from-string
    setl lispwords+=with-output-to-string,handle-exceptions,call/cc,rec,receive
    setl lispwords+=call-with-output-file
endif
