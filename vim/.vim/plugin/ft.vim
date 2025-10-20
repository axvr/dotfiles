" Ftplugin config.
let g:tex_flavor = "latex"
let g:markdown_minlines = 200
let g:ledger_is_hledger = v:true
let g:org_clean_folds = 1

augroup axvr/filetype_config
    autocmd!
    autocmd FileType c,cpp,go,gitconfig,fstab setl noet sts=8 sw=8
    autocmd FileType lisp,clojure,scheme,json,ruby,markdown setl et sts=2 sw=2
    autocmd FileType html,css setl noet sts=2 sw=2 ts=2
    autocmd FileType perl,sh,python,javascript setl tw=79
    autocmd FileType lisp,clojure,scheme setl commentstring=;;%s cpo-=J
    autocmd FileType robots,crontab,spec,desktop,execline setl commentstring=#%s
    autocmd FileType c,cpp setl path+=/usr/include
    autocmd FileType tex compiler latexmk
    autocmd FileType gitcommit setl spell
    autocmd FileType git,gitcommit,diff setl foldmethod=syntax foldlevel=100
    autocmd FileType sh,bash setl keywordprg=:Man | compiler shellcheck
augroup END
