" Vim Plugin Configuration
" ========================

scriptencoding utf-8

" Plugin Setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone https://github.com/axvr/Vivid.vim.git ~/.vim/pack/vivid/opt/Vivid.vim
    endif
    packadd Vivid.vim
endif

" Vim enhancements
Plugin 'jiangmiao/auto-pairs', { 'enabled': 1, } " :h AutoPairs.txt
Plugin 'tommcdo/vim-lion',     { 'enabled': 1, } " :h lion.txt
let g:lion_squeeze_spaces = 1
Plugin 'wellle/targets.vim',   { 'enabled': 1, } " :h targets.txt
Plugin 'romainl/vim-cool',     { 'enabled': 1, }
Plugin 'romainl/vim-qf',       { 'enabled': 1, } " :h qf.txt
packadd matchit
runtime ftplugin/man.vim

" Git integration
Plugin 'tpope/vim-fugitive'     " :h fugitive.txt
Plugin 'mhinz/vim-signify'      " :h signify.txt
let g:signify_vcs_list               = ['git', 'hg']
let g:signify_realtime               = 1
let g:signify_sign_add               = '+'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '•'
let g:signify_sign_delete            = '_'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_show_count        = 0

" Syntax highlighting & formatting packs
Plugin 'rust-lang/rust.vim'
Plugin 'ledger/vim-ledger'

" Colour schemes and themes
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1, }

" Netrw Configuration
let g:netrw_banner    = 0
let g:netrw_winsize   = 20


" Git Plugin Enabling
function! s:enable_vcs_plugins() abort
    if system('parse_vcs_branch') !=# ''
        call vivid#enable('vim-fugitive', 'vim-signify')
    endif
endfunction
autocmd! BufReadPre * call s:enable_vcs_plugins()

