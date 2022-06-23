vim9script

b:repl_config = {
      'cmd': filereadable('project.clj') ? 'lein repl' : 'clj-socket',
      'rlwrap': filereadable('project.clj') ? 0 : 1,
      'load_file': '(load-file "%s")'
    }

g:clojure_discard_macro = 1

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

setlocal keywordprg=:Doc
compiler clj-kondo

setlocal suffixesadd=.clj,.cljc,.cljs,.cljx,.edn
setlocal includeexpr=clojure#FormatNsAsPath(v:fname)

# TODO: remap definition lookups.  (e.g. <C-]>)

nnoremap gzn :<C-u>Ns<CR>

# TODO: dedicated :Lint and :Test commands.
# TODO: make :Source default to symbol under cursor.
