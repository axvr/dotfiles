" =============================================================
" Description:  Configure Plugins for Vim to use
" File:         ~/.vim/plugin/plugins.vim
" =============================================================

" Plugin setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone "https://github.com/axvr/vivid.vim" "$HOME/.vim/pack/vivid/opt/Vivid.vim"
    endif
    packadd Vivid.vim
endif

" Vim enhancements
let g:netrw_banner = 0
Plugin 'tommcdo/vim-lion',     { 'enabled': 1 }
let g:lion_squeeze_spaces = 1
Plugin 'romainl/vim-cool',     { 'enabled': 1 }
Plugin 'romainl/vim-qf',       { 'enabled': 1 }
Plugin 'tpope/vim-commentary', { 'enabled': 1 }
packadd matchit

" Syntax highlighting & formatting packs
Plugin 'ledger/vim-ledger'
Plugin 'OmniSharp/omnisharp-vim'
let g:tex_flavor = "latex"

" Colour schemes and themes
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1 }

" Simple way to test out plugins
command! -nargs=1 -bar PluginTest call <SID>vivid_test(<args>)
function! s:vivid_test(url) abort
    let l:name = vivid#add(a:url, { 'enabled': 1 })
    exec 'autocmd VimLeavePre * call vivid#clean("'.l:name.'")'
endfunction
