" Description:  Clj-Kondo linter integration.
" File:         compiler/clj-kondo.vim

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'clj-kondo'

setlocal makeprg=clj\ -M:kondo\ --lint
setlocal errorformat=%f:%l:%c:\ %trror:\ %m,
                    \%f:%l:%c:\ %tarning:\ %m,
                    \%-G%m

let &cpo = s:cpo_save
unlet s:cpo_save
