" =============================================================
" Description:  Configure Plugins for Vim to use
" File:         ~/.vim/plugin/plugins.vim
" =============================================================

" Plugin Setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone "https://github.com/axvr/vivid.vim" "$HOME/.vim/pack/vivid/opt/Vivid.vim"
    endif
    packadd Vivid.vim
endif

" Vim enhancements
Plugin 'tommcdo/vim-lion',     { 'enabled': 1 }
let g:lion_squeeze_spaces = 1
Plugin 'romainl/vim-cool',     { 'enabled': 1 }
Plugin 'romainl/vim-qf',       { 'enabled': 1 }
Plugin 'tpope/vim-commentary', { 'enabled': 1 }
Plugin 'tpope/vim-surround',   { 'enabled': 1 }
Plugin 'tpope/vim-vinegar',    { 'enabled': 1 }
Plugin 'mbbill/undotree',      { 'command': ['UndotreeToggle', 'UndotreeShow'] }
packadd matchit

" VCS integration
Plugin 'mhinz/vim-signify', { 'enabled': 1 }
let g:signify_vcs_list               = ['git', 'hg']
let g:signify_realtime               = 0
let g:signify_sign_add               = '+'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '•'
let g:signify_sign_delete            = '_'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_show_count        = 0
Plugin 'rhysd/committia.vim'

" Syntax highlighting & formatting packs
Plugin 'ledger/vim-ledger'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'leafgarland/typescript-vim'

" Colour schemes and themes
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1 }

" Simple way to test out plugins
" TODO integrate into Vivid, or add to the docs
command! -nargs=1 -bar PluginTest call <SID>vivid_test(<args>)
function! s:vivid_test(url) abort
    let l:name = vivid#add(a:url, { 'enabled': 1 })
    exec 'autocmd VimLeavePre * call vivid#clean("'.l:name.'")'
endfunction
