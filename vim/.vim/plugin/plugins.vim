" =============================================================
" Description:  Configure Plugins for Vim to use
" File:         ~/.vim/config/plugins.vim
" =============================================================

" Plugin Setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone https://github.com/axvr/Vivid.vim.git ~/.vim/pack/vivid/opt/Vivid.vim
    endif
    packadd Vivid.vim
endif

" Vim enhancements
Plugin 'tommcdo/vim-lion',     { 'enabled': 1, }
let g:lion_squeeze_spaces = 1
Plugin 'wellle/targets.vim',   { 'enabled': 1, }
Plugin 'romainl/vim-cool',     { 'enabled': 1, }
Plugin 'romainl/vim-qf',       { 'enabled': 1, }
packadd matchit
runtime ftplugin/man.vim
autocmd! FileType man setlocal textwidth=0 nofoldenable

" VCS integration
Plugin 'itchyny/vim-gitbranch', { 'enabled': 1, }
Plugin 'mhinz/vim-signify'
let g:signify_vcs_list               = ['git', 'hg']
let g:signify_realtime               = 1
let g:signify_sign_add               = '+'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '•'
let g:signify_sign_delete            = '_'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_show_count        = 0
Plugin 'rhysd/committia.vim'

" Syntax highlighting & formatting packs
Plugin 'rust-lang/rust.vim'
Plugin 'ledger/vim-ledger'
Plugin 'OrangeT/vim-csharp'

" Colour schemes and themes
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1, }

" Netrw Configuration
let g:netrw_banner    = 0
let g:netrw_winsize   = 20

" Tex.vim Syntax plugin Config
let g:tex_flavor = "latex"

" VCS Plugin Enabling
function! s:enable_vcs_plugins() abort
    if gitbranch#name() !=# ''
        call vivid#enable('vim-signify', 'committia.vim')
    endif
endfunction
autocmd! BufReadPre * call s:enable_vcs_plugins()

