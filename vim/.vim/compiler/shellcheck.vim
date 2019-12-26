" =============================================================
" Description:  ShellCheck compiler integration.
" File:         ~/.vim/compiler/shellcheck.vim
" =============================================================

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'shellcheck'

setlocal makeprg=shellcheck\ -f\ gcc\ %

setlocal efm=%f:%l:%c:\ %trror:\ %m,
            \%f:%l:%c:\ %tarning:\ %m,
            \%f:%l:%c:\ note:\ %m,
            \%f:%l:%c:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save
