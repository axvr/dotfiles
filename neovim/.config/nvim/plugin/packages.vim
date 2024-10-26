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

let g:sandwich_no_default_key_mappings = 1
packadd sandwich
" Switch Sandwich to use "S" instead of "s".
nmap S <Nop>
xmap S <Nop>
" add
nmap Sa <Plug>(sandwich-add)
xmap Sa <Plug>(sandwich-add)
omap Sa <Plug>(sandwich-add)
" delete
nmap Sd <Plug>(sandwich-delete)
xmap Sd <Plug>(sandwich-delete)
nmap Sdb <Plug>(sandwich-delete-auto)
" replace
nmap Sr <Plug>(sandwich-replace)
xmap Sr <Plug>(sandwich-replace)
nmap Srb <Plug>(sandwich-replace-auto)
" text-objects (if you need)
omap iS <Plug>(textobj-sandwich-auto-i)
xmap iS <Plug>(textobj-sandwich-auto-i)
omap aS <Plug>(textobj-sandwich-auto-a)
xmap aS <Plug>(textobj-sandwich-auto-a)
omap iS <Plug>(textobj-sandwich-query-i)
xmap iS <Plug>(textobj-sandwich-query-i)
omap aS <Plug>(textobj-sandwich-query-a)
xmap aS <Plug>(textobj-sandwich-query-a)

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
let g:dirvish_mode = ':sort | silent! g,\v/\.DS_Store$,d _'
packadd dirvish

packadd conjure
packadd ionide

let g:colortemplate_toolbar = 0

let g:tex_flavor = "latex"
let g:markdown_minlines = 200

let g:ledger_is_hledger = v:true
