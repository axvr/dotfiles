" =============================================================
" Description:  Vim Configuration File
" File:         ~/.vimrc
" =============================================================

" Essentials
set encoding=utf-8
filetype plugin indent on
if !exists('g:syntax_on')
    syntax enable
endif
set hidden
set autoread
set mouse=a
set backspace=indent,eol,start
set spelllang=en_gb

" Styling
set showcmd
set ruler
set cursorline
let &colorcolumn='+'.join(range(1,256), ',+')
set belloff=all
set showmatch
set lazyredraw

" Searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set wrapscan

" Backup, Swap & Undo files
let s:dirs = [expand($HOME.'/.vim/backup'),
            \ expand($HOME.'/.vim/swap'),
            \ expand($HOME.'/.vim/undo')]
for s:dir in s:dirs
    if !isdirectory(s:dir)
        call mkdir(s:dir, 'p')
    endif
endfor
let &backupdir = s:dirs[0]
set backup
let &directory = s:dirs[1]
let &undodir = s:dirs[2]
set undofile

" Set leader and localleader keys
let mapleader = " "
let maplocalleader = " m"

" Vim Omnicomplete, Ins-complete & Wild menu
set omnifunc=syntaxcomplete#Complete
set wildmenu
set wildignore+=*/tmp/*,*.so,*.pyc,*.db,*.sqlite,*/node_modules/*,*/.git/*,*.dll,*.exe,*.cache,*.o
set path+=**

" Indentation config
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab
set shiftround
set autoindent

" Line wrap config (Soft wrap code, hard wrap comments and text)
let &showbreak='+++ '
set wrap
set linebreak
set nolist
set breakindent

" Plugin setup
if has('vim_starting')
    if !filereadable(expand($HOME . '/.vim/pack/vivid/opt/Vivid.vim/autoload/vivid.vim'))
        silent !git clone "https://github.com/axvr/vivid.vim" "$HOME/.vim/pack/vivid/opt/Vivid.vim"
    endif
    packadd Vivid.vim
endif

let g:netrw_banner = 0
let g:tex_flavor = "latex"

packadd matchit
Plugin 'ledger/vim-ledger'
Plugin 'OmniSharp/omnisharp-vim'
if v:version <= 800
    Plugin 'nickspoons/vim-cs'
endif
Plugin 'liuchengxu/space-vim-dark', { 'enabled': 1 }

" Simple way to test out plugins
" TODO move to vivid documentation
command! -nargs=1 -bar PluginTest call <SID>vivid_test(<args>)
function! s:vivid_test(url) abort
    let l:name = vivid#add(a:url, { 'enabled': 1 })
    exec 'autocmd VimLeavePre * call vivid#clean("'.l:name.'")'
endfunction

" Fix displaying of colours in terminal
if &term =~# '^.*256color$'
    set termguicolors
endif
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

colorscheme space-vim-dark

" Display additional file information
nnoremap <Leader>fi :<C-u>echo &fenc?&fenc:&enc '' &ff '' &ft<CR>

" Easily convert file formats
" Unix --> Dos, Dos --> Unix, Mac --> Unix
function! s:Convert() abort
    let l:ff = &fileformat
    update
    edit ++fileformat=dos
    if l:ff !=# 'unix'
        setlocal fileformat=unix
    endif
endfunction
command! -nargs=0 -bar ConvertFileFormat :call <SID>Convert()

" Find all TODOs in current repository
function! s:TODOs()
    let l:old_grepprg = &l:grepprg
    setlocal grepprg=todos
    silent grep
    let &l:grepprg = l:old_grepprg
endfunction
command! -nargs=0 -bar TODOs :call <SID>TODOs()

augroup filetypes
    autocmd!
    autocmd FileType ledger call vivid#enable('vim-ledger')
    autocmd FileType c,make,go,gitconfig,help setlocal noet sts=8 sw=8 ts=8
    autocmd FileType lisp,json,ruby setlocal et sts=2 sw=2 ts=8
    autocmd FileType perl,sh,python,haskell,javascript setlocal tw=79
    autocmd FileType gitcommit setlocal spell
    autocmd FileType tex setlocal mp=latexmk\ -pdf\ %
    autocmd FileType perl compiler perl
    autocmd FileType sh setlocal mp=shellcheck\ -f\ gcc\ %
    autocmd FileType sh setlocal efm=%f:%l:%c:\ %trror:\ %m,
                \%f:%l:%c:\ %tarning:\ %m,
                \%f:%l:%c:\ note:\ %m,
                \%f:%l:%c:\ %m
augroup END
