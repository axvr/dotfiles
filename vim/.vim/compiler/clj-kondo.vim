" Clj-Kondo linter integration.

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'clj-kondo'

setlocal makeprg=clj-kondo\ --lint\ src:test:resources
setlocal errorformat=%f:%l:%c:\ %trror:\ %m,
                    \%f:%l:%c:\ %tarning:\ %m,
                    \%-G%m

let &cpo = s:cpo_save
unlet s:cpo_save
