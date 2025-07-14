" Clj-Kondo linter integration.

let current_compiler = 'clj-kondo'
setlocal makeprg=clj-kondo\ --lint\ src:test:resources
setlocal errorformat=%f:%l:%c:\ %trror:\ %m,
                    \%f:%l:%c:\ %tarning:\ %m,
                    \%-G%m
