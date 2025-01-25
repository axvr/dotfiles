" Configure packages.

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
            \ for s:dir in glob('~/.config/nvim/pack/*/*/*/doc', 1, 1)
            \ |   execute 'helptags ' . s:dir
            \ | endfor
            \ | unlet s:dir

" Enable the built-in Man page viewer.
runtime ftplugin/man.vim
set keywordprg=:Man

packadd matchit
packadd traces
packadd qf
packadd fugitive

if has('nvim')
    packadd conjure
endif

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
let g:dirvish_mode = ':sort | silent! g,\v/\.DS_Store$,d _'
packadd dirvish

let g:colortemplate_toolbar = 0
let g:tex_flavor = "latex"
let g:markdown_minlines = 200
let g:ledger_is_hledger = v:true
