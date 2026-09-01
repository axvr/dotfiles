setlocal nojoinspaces
setlocal keywordprg=:CljDoc
setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.cljr,.edn,.bb,.clj_kondo
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

if isdirectory('.clj-kondo') | compiler clj-kondo | endif

let g:clojure_discard_macro = 0

let b:repl_config = {
    \   'cmd': filereadable('project.clj') ? 'lein repl' : 'clj-socket',
    \   'load_file': '(clojure.core/load-file "%s")'
    \ }

com! -buffer       -bar -nargs=* CljNs        :call clojure#ChangeNs(<q-args>)
com! -buffer       -bar -nargs=1 CljDir       :call clojure#Dir(<q-args>)
com! -buffer       -bar -nargs=+ CljFindDoc   :call clojure#FindDoc(<q-args>)
com! -buffer -bang -bar -nargs=1 CljRequire   :call clojure#Require(<q-args>, (<q-bang> ==# '!'))
com! -buffer       -bar -nargs=1 CljImport    :call clojure#Import(<q-args>)
com! -buffer       -bar -nargs=1 CljUse       :call clojure#Use(<q-args>)
com! -buffer       -bar -nargs=? CljNsUnmap   :call clojure#NsUnmap('*ns*', <q-args>)
com! -buffer       -bar -nargs=? CljNsUnalias :call clojure#NsUnalias('*ns*', <q-args>)

com! -buffer       -bar -nargs=? -complete=customlist,clojure#CmdComplete CljDoc     :call clojure#Doc(<q-args>)
com! -buffer       -bar -nargs=? -complete=customlist,clojure#CmdComplete CljSource  :call clojure#Source(<q-args>)
com! -buffer       -bar -nargs=1 -complete=customlist,clojure#CmdComplete CljApropos :call clojure#Apropos(<q-args>)

" TODO: remap definition lookups.  (e.g. <C-]>)
nnoremap gzn :<C-u>CljNs<CR>
