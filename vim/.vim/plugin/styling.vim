" =============================================================
" Description:  Configure styling for Vim
" File:         ~/.vim/plugin/styling.vim
" =============================================================

set showcmd
set ruler
set cursorline
let &colorcolumn='+'.join(range(1,256), ',+')
set belloff=all

if &term =~# '^.*256color$'
    set termguicolors
endif
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

autocmd! ColorScheme space-vim-dark highlight SpellBad   ctermbg=NONE
autocmd! ColorScheme space-vim-dark highlight SpellLocal ctermbg=NONE

colorscheme space-vim-dark
