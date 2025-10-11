" Configure packages.

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
            \ call glob('~/.vim/pack/*/*/*/doc', 1, 1)
            \ ->add(expand('~/.vim/doc'))
            \ ->foreach("exec 'helptags' v:val")

" Enable the built-in Man page viewer.
runtime ftplugin/man.vim
set keywordprg=:Man

packadd qf
packadd matchit
packadd traces
packadd fugitive
packadd info

if has('nvim')
    packadd commentary
    packadd conjure
else
    packadd comment
    packadd unimpaired
    packadd editorconfig
endif

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
let g:dirvish_mode = ':sort | silent! g,\v/\.DS_Store$,d _'
packadd dirvish

let g:qf_number = 0
let g:tex_flavor = "latex"
let g:markdown_minlines = 200
let g:ledger_is_hledger = v:true
let g:org_clean_folds = 1
