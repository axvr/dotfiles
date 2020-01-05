" Scheme configuration for Vim

highlight! def link schemeParentheses Delimiter

" https://wiki.call-cc.org/vim

let b:is_chicken = 1

if get(b:, "is_chicken", 0) || get(g:, "is_chicken", 0)

    " " Indentation
    setl lispwords+=let-values,condition-case,with-input-from-string
    setl lispwords+=with-output-to-string,handle-exceptions,call/cc,rec,receive
    setl lispwords+=call-with-output-file

    " " Indent toplevel sexp.
    function! Scheme_indent_top_sexp() abort
        let pos = getpos('.')
        silent! exec "normal! 99[(=%"
        call setpos('.', pos)
    endfunction
    nnoremap <buffer> <silent> == :call Scheme_indent_top_sexp()<cr>

    " REPL using Tmux

    function! Scheme_send_sexp(sexp) abort
        let ss = escape(a:sexp, '\"')
        "call system("screen -p csi -X stuff \"" . ss . "\n\"")
        call system("tmux send-keys -t csi \"" . ss . "\n\"")
    endfunction

    function! Scheme_eval_defun() abort
        let pos = getpos('.')
        silent! exec "normal! 99[(yab"
        call Scheme_send_sexp(@")
        call setpos('.', pos)
    endfunction

    nnoremap <buffer> <silent> <localleader>s :call Scheme_eval_defun()<cr>
    nnoremap <buffer> <silent> <localleader>f :call Scheme_send_sexp("(load \"" . expand("%:p") . "\")\n")<cr>

endif
