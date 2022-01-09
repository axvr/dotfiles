let b:repl_config = { 'cmd': 'clj-socket', 'load_file': '(load-file "%s")' }

let g:clojure_discard_macro = 1

command! -buffer -bar -nargs=* Ns        :call clojure#change_ns(<q-args>)
command! -buffer -bar -nargs=1 Doc       :call clojure#doc(<q-args>)
command! -buffer -bar -nargs=1 Source    :call clojure#source(<q-args>)
command! -buffer -bar -nargs=1 Apropos   :call clojure#apropos(<q-args>)
command! -buffer -bar -nargs=1 NsPublics :call clojure#ns_publics(<q-args>)

command! -buffer -bang -bar -nargs=1 Require :call clojure#require(<q-args>, (<q-bang> ==# '!'))
command! -buffer -bar -nargs=1 Import        :call clojure#import(<q-args>)
command! -buffer -bar -nargs=1 Use           :call clojure#use(<q-args>)

command! -buffer -bar -nargs=1 NsUnmap   :call clojure#ns_unmap('*ns*', <q-args>)
command! -buffer -bar -nargs=1 NsUnalias :call clojure#ns_unalias('*ns*', <q-args>)

setlocal keywordprg=:Doc
compiler clj-kondo

" FIXME: Create issue to add option to prioritise suffix checks over dir check
setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.edn
setlocal includeexpr=clojure#format_ns_as_file_path(v:fname)

" TODO: remap definfition lookups.

nnoremap gzn :<C-u>Ns<CR>

" TODO: dedicated :Lint and :Test commands.
" TODO: make :Source default to symbol under cursor.
