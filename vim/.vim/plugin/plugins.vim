" =============================================================
" Description:  Configure Plugins for Vim to use
" File:         ~/.vim/plugin/plugins.vim
" =============================================================

" Plugin Setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone https://github.com/axvr/Vivid.vim.git ~/.vim/pack/vivid/opt/Vivid.vim
    endif
    packadd Vivid.vim
endif

" Vim enhancements
Plugin 'tommcdo/vim-lion',     { 'enabled': 1 }
let g:lion_squeeze_spaces = 1
Plugin 'wellle/targets.vim',   { 'enabled': 1 }
Plugin 'romainl/vim-cool',     { 'enabled': 1 }
Plugin 'romainl/vim-qf',       { 'enabled': 1 }
Plugin 'tpope/vim-commentary', { 'enabled': 1 }
Plugin 'mbbill/undotree'
packadd matchit

" VCS integration
Plugin 'tpope/vim-fugitive',    { 'enabled': 1 }
Plugin 'mhinz/vim-signify',     { 'enabled': 1 }
let g:signify_vcs_list               = ['git', 'hg']
let g:signify_realtime               = 0
let g:signify_sign_add               = '+'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '•'
let g:signify_sign_delete            = '_'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_show_count        = 0
Plugin 'rhysd/committia.vim'

Plugin 'axvr/dotnet.vim', { 'enabled': 1 }
" Syntax highlighting & formatting packs
Plugin 'editorconfig/editorconfig-vim', { 'enabled': 1 }
Plugin 'ledger/vim-ledger'
" TypeScript
Plugin 'leafgarland/typescript-vim'
Plugin 'Quramy/tsuquyomi'

" Colour schemes and themes
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1 }
Plugin 'robertmeta/nofrils'

" Tex.vim Syntax plugin Config
let g:tex_flavor = "latex"

" FIXME VCS Plugin Enabling
" function! s:enable_vcs_plugins() abort
"     if exists('b:git_dir')
"         call vivid#enable('vim-signify')
"     endif
" endfunction
" autocmd! DirChanged,BufReadPre * call s:enable_vcs_plugins()
" autocmd! BufNewFile,BufReadPost,BufEnter * call s:enable_vcs_plugins()


" Enable plugins on Commands (TODO move to Vivid)
function! VividCommand(plugin, ...) abort
    for l:cmd in a:000
        execute 'command '.l:cmd.' :call vivid#enable("'.a:plugin.'") | '.l:cmd
    endfor
endfunction

call VividCommand('undotree',
            \ 'UndotreeToggle', 'UndotreeShow', 'UndotreeHide', 'UndotreeFocus')


" PowerShell Syntax highlighting
File 'PProvost/vim-ps1/master/syntax/ps1.vim', '~/.vim/syntax/powershell.vim'
