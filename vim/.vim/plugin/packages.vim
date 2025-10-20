" Summary: Configure and enable packages.
" Help:    :help packages

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
            \ call glob('~/.vim/pack/*/*/*/doc', 1, 1)
            \ ->add(expand('~/.vim/doc'))
            \ ->foreach("exec 'helptags' v:val")

" Add `:Man` and `:Info`.
runtime ftplugin/man.vim
set keywordprg=:Man
packadd info

packadd qf
let g:qf_number = 0

packadd matchit
packadd fugitive
if has('nvim')
    packadd commentary
    packadd conjure
else
    packadd comment
    packadd traces
    packadd unimpaired
    packadd editorconfig
endif
