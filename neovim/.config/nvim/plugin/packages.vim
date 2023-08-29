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

packadd conjure

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
packadd dirvish
let g:dirvish_mode = ':sort | g,\v/\.DS_Store$,d'

let g:terraform_fmt_on_save = 1
