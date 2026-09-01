let g:clojure_discard_macro = 1

if isdirectory('.clj-kondo')
    compiler clj-kondo
endif

setlocal keywordprg=:Doc
setlocal nojoinspaces
setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.cljr,.edn,.bb,.clj_kondo
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

if has('nvim')
    nmap <buffer> <C-]> <LocalLeader>gd
endif

let b:repl_config = {
\   'cmd': filereadable('project.clj') ? 'lein repl' : 'clj-socket',
\   'load_file': '(clojure.core/load-file "%s")'
\ }

command! -buffer -bar -nargs=* Ns      :call clojure#ChangeNs(<q-args>)
command! -buffer -bar -nargs=1 Dir     :call clojure#Dir(<q-args>)
command! -buffer -bar -nargs=+ FindDoc :call clojure#FindDoc(<q-args>)
command! -buffer -bar -nargs=1 -complete=customlist,clojure#CmdComplete Doc
            \ :call clojure#Doc(<q-args>)
command! -buffer -bar -nargs=1 -complete=customlist,clojure#CmdComplete Source
            \ :call clojure#Source(<q-args>)
command! -buffer -bar -nargs=1 -complete=customlist,clojure#CmdComplete Apropos
            \ :call clojure#Apropos(<q-args>)

command! -buffer -bang -bar -nargs=1 Require :call clojure#Require(<q-args>, (<q-bang> ==# '!'))
command! -buffer -bar -nargs=1 Import        :call clojure#Import(<q-args>)
command! -buffer -bar -nargs=1 Use           :call clojure#Use(<q-args>)

command! -buffer -bar -nargs=1 NsUnmap   :call clojure#NsUnmap('*ns*', <q-args>)
command! -buffer -bar -nargs=1 NsUnalias :call clojure#NsUnalias('*ns*', <q-args>)

" # TODO: remap definition lookups.  (e.g. <C-]>)

nnoremap gzn :<C-u>Ns<CR>

" # TODO: dedicated :Lint and :Test commands.
" # TODO: make :Source default to symbol under cursor.
