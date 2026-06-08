" Summary: Configure and enable packages.
" Help:    :help packages

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
    \ call glob('~/.vim/pack/*/*/*/doc', 1, 1)
    \ ->add(expand('~/.vim/doc'))
    \ ->foreach("exec '<mods> helptags' v:val")

" Regenerate spell files from word lists.
command! -nargs=0 -bar -bang Mkspell
    \ call glob('~/.vim/spell/*', 1, 1)
    \ ->foreach("exec '<mods> mkspell<bang>' v:val")
silent! Mkspell

" Add `:Man` and `:Info`.
runtime ftplugin/man.vim
set keywordprg=:Man
packadd info

packadd qf
let g:qf_number = 0

let g:fugitive_legacy_commands = v:false
packadd fugitive

packadd matchit

" Vim 9.1 has a built-in "comment" package.  Fallback to "commentary".
try | packadd comment | catch | packadd commentary | endtry

if has('nvim')
    let g:conjure#filetypes = ['clojure']
    packadd conjure
else
    packadd traces
    packadd unimpaired

    " Vim 9.1 adds a built-in "editorconfig" package.
    silent! packadd editorconfig
endif
