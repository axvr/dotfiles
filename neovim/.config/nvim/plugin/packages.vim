" Configure packages.

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
            \ for s:dir in glob('~/.config/nvim/pack/*/*/*/doc', 1, 1)
            \ |   execute 'helptags ' . s:dir
            \ | endfor
            \ | unlet s:dir

runtime ftplugin/man.vim
set keywordprg=:Man

packadd matchit
packadd traces
packadd qf

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
packadd dirvish
let g:dirvish_mode = ':sort | silent! g,\v/\.DS_Store$,d'

packadd conjure
if ! get(g:, 'conjure#client#clojure#nrepl#connection#auto_repl#cmd', 0)
    let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'clojure -M:repl/nrepl'
endif

packadd ionide
