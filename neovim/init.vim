" Neovim Configuration File ($XDG_CONFIG_HOME/nvim/init.vim)
" ==========================================================

" Brief help
" ----------
" :PlugStatus       - View status of plugins
" :PlugInstall      - Install new plugins
" :PlugClean        - Remove unused plugins
" :PlugUpdate       - Update installed plugins
" :PlugUpgrade      - Upgrade Vim-Plug
" :source $MYVIMRC  - Load latest version of init.vim

" Encoding
scriptencoding utf-8
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

function! s:get_SID()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_zeget_SID$')
endfunction
let s:SID = s:get_SID()
delfunction s:get_SID


" -----------------------------------------------------------------------------
" TODO line & tab length
" TODO mode line
" TODO Auto-complete for all languages wanted
" TODO change configuration based upon file type
" TODO custom Vim + airline theme (change TODO colour)
" TODO replace syntastic
" TODO improve Vim buffers (similar to SM)
" TODO improve Vim vimrc
" TODO try to use same tools (e.g. linters) as SM
" TODO spelling fix
" TODO tidy up this document
"
" Languages still to optimise for
" * C++
" * C
" * Rust
" * Python
" * Java
" * Bash / Shell
" * Perl
" * JavaScript
" * HTML
" * CSS
" -----------------------------------------------------------------------------

" Plugin Setup
" ------------

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif

call plug#begin()
  " Input Plugins Below this Line {{{

  " File viewers and switchers
  Plug 'ctrlpvim/ctrlp.vim',                                               " CtrlP Fuzzy Finder            <-- :help ctrlp.txt
  Plug 'scrooloose/nerdtree',           { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }    " NERDTree Plugin               <-- :help NERD_tree.txt
  Plug 'Xuyuanp/nerdtree-git-plugin',   { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }    " Display Git Diffs in NERDTree

  " Auto-complete
  Plug 'jiangmiao/auto-pairs'                                                           " Smart brackets and quotes
  Plug 'Shougo/deoplete.nvim',          { 'do': ':UpdateRemotePlugins' }
  Plug 'Shougo/neco-vim',               { 'for': 'vim' }                                " VimL completion
  Plug 'zchee/deoplete-clang'                                                           " Clang completion engine
  Plug 'Shougo/neco-syntax'                                                             " Many languages simple completion engine
  Plug 'poppyschmo/deoplete-latex',     { 'for': 'tex' }                                " Experimental LaTeX auto-completion engine
  " TODO Python completion engine 'zchee/deoplete-jedi'
  " TODO Rust completion engine 'sebastianmarkow/deoplete-rust'
  " TODO others

  " TODO Syntax checking (linting)
  "Plug 'scrooloose/syntastic'           " Syntastic Syntax Checker Plugin   <-- :help syntastic
  "Plug 'neomake/neomake'

  " Code formatting
  Plug 'rhysd/vim-clang-format', { 'on': ['ClangFormat', 'ClangFormatAutoToggle', 'ClangFormatAutoEnable'] }  " Format files using Clang

  " Vim enhancements
  Plug 'terryma/vim-multiple-cursors'   " Vim Multiple Cursors Plugin       <-- :help vim-multiple-cursors
  Plug 'rhysd/clever-f.vim'
  Plug 'majutsushi/tagbar'              " Display Tags of a File Easily     <-- :help tagbar
  Plug 'jceb/vim-orgmode'               " :help orgguide
  Plug 'tpope/vim-speeddating'          " Increment dates

  " Git integration
  Plug 'tpope/vim-fugitive'             " Fugitive.Vim Git Wrapper Plugin   <-- :help fugitive
  Plug 'airblade/vim-gitgutter'         " Show a Git Diff in the 'Gutter'   <-- :help GitGutter
  Plug 'rhysd/committia.vim'            " More Pleasant Editing on Commit Message

  " Syntax highlighting packs
  Plug 'rust-lang/rust.vim'             " Rust Syntax Highlighting
  Plug 'tomlion/vim-solidity'           " Solidity Syntax Highlighting
  Plug 'lervag/vimtex', { 'for': 'tex' }

  " Colourschemes and themes
  Plug 'itchyny/lightline.vim'          " Lightline Theme Plugin
  "Plug 'vim-airline/vim-airline'        " Airline Theme Plugin              <-- :help Airline
  "Plug 'vim-airline/vim-airline-themes' " Airline Theme Packages
  "Plug 'rafi/awesome-vim-colorschemes'  " Colour Schemes for Vim
  Plug 'jacoborus/tender.vim'
  Plug 'liuchengxu/space-vim-dark'
  Plug 'kristijanhusak/vim-hybrid-material'

  " Input Plugins Below this line }}}
call plug#end()


" -----------------------------------------------------------------------------

" Basic Configuration
" -------------------

" Searching
set ignorecase                  " Ignore case in searches
set smartcase                   " Enables smart case mode
set hlsearch                    " Highlight all search results
set incsearch                   " Searches for strings incrementaly
set wrapscan                    " Allow searching of first real match

" TODO Mode line
set modeline

" Undo
if has('persistent_undo')
    if !isdirectory($HOME . '/.config/nvim/undo')
        call mkdir($HOME . '/.config/nvim/undo', 'p')
    endif
    set undodir=$HOME/.config/nvim/undo
    set undofile
else
    set undolevels=1000
endif

set confirm                     " confirmation prompts
set fileformats=unix,dos,mac
set mouse=a                     " Enable full mouse support
set updatetime=250
set backspace=indent,eol,start  " Backspace behaviour: current line only
set history=100
set ruler                       " show row and col ruler info
set showmatch                   " Highlight matching brackets
set foldenable                  " Enable folding
set foldmethod=marker           " Set fold method to {{{ & }}}
autocmd BufWritePre * %s/\s\+$//e " Remove trailing whitespace

" Vim & GVim styling
set number relativenumber " Show the line numbers
set noshowmode
set laststatus=2
set cursorline " Highlight current line
let &colorcolumn=join(range(81,335),",")
set visualbell t_vb=            " Disable sound alerts

set background=dark
colorscheme tender
"hi Normal guibg=NONE ctermbg=NONE
if (has("termguicolors"))
  set termguicolors
endif
if (has('gui_running'))
  set guifont=Monospace\ 11
endif

" Vim Omnicomplete
autocmd BufNewFile,BufRead,BufEnter *.cpp,*.hpp
                        \ set omnifunc=omni#cpp#complete#Main
set completeopt=longest,menuone,menu
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
                        \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
                        \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" Wild menu
set wildmenu
set omnifunc=syntaxcomplete#Complete
set path+=**
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite


" -----------------------------------------------------------------------------

" Set Keymaps & Commands
" ----------------------

" Set Leader key
let g:mapleader = "\\"

" Spacemacs style leader keybindings
nnoremap <leader>fs :<C-u>w<CR>
nnoremap <leader>ff :<C-u>CtrlP<CR>
nnoremap <leader>ft :<C-u>NERDTreeToggle<CR>
nnoremap <leader>qq :<C-u>qa<CR>
nnoremap <leader>gs :<C-u>Gstatus<CR>
nnoremap <leader>gc :<C-u>Gcommit<CR>
nnoremap <leader>gd :<C-u>Gdiff<CR>
nnoremap <leader>ge :<C-u>Gedit<CR>
nnoremap <leader>gm :<C-u>Gmove<CR>
nnoremap <leader>gr :<C-u>Gdelete<CR>
nnoremap <leader>gb :<C-u>Gblame<CR>
nnoremap <leader>'  :<C-u>terminal<CR>
" Spell check
nnoremap <leader>st :<C-u>setlocal spell! spelllang=en_gb<CR>
nnoremap <F7> :<C-u>setlocal spell! spelllang=en_gb<CR>
" Switch tabs using <FX> keys
nnoremap <F5> :<C-u>tabp<CR>
nnoremap <F6> :<C-u>tabn<CR>
" Make tags file using ctags
command! MakeTags !ctags -R .
" Tagbar
nnoremap <silent> <F4> :<C-u>TagbarToggle<CR>
" NERDTree
nnoremap <silent> <F2> :<C-u>NERDTreeFind<CR>
nnoremap <F3> :<C-u>NERDTreeToggle<CR>
" Clang format
autocmd FileType c,h,cpp,hpp,cc,objc setlocal
      \ nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,h,cpp,hpp,cc,objc setlocal
      \ vnoremap <buffer><Leader>cf :ClangFormat<CR>

function! s:delete_current_buf()
    let bufnr = bufnr('%')
    bnext
    if bufnr == bufnr('%') | enew | endif
    silent! bdelete! #
endfunction
nnoremap <leader>bd :<C-u>call <SID>delete_current_buf()<CR>

" Show Highlighting group for current word
function! s:syn_stack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction
nnoremap <leader>hg :call <SID>syn_stack()<CR>


" -----------------------------------------------------------------------------

" Plugin Configurattion
" ---------------------

" Airline Config
"let g:airline#extensions#tagbar#enabled = 1
"let g:airline#extensions#syntastic#enabled = 1
"let g:airline_theme='lucius' " Set Airline theme

" Lightline Config
let g:lightline = {
      \ 'colorscheme': 'tender',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head',
      \ },
      \ }

" CtrlP Config
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_prompt_mappings = {
      \ 'AcceptSelection("e")': ['<c-t>'],
      \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
      \ }

" NERDTree Config
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 25

" Clever-f Config
let g:clever_f_smart_case = 1
let g:clever_f_across_no_line = 1

" Clang Format Config
" TODO Java, JavaScript, Obj-C, C
let g:clang_format#code_style = 'google'

" Deoplete Config
let g:deoplete#enable_at_startup = 1
" Deoplete-Clang - find locations: https://github.com/zchee/deoplete-clang
let g:deoplete#sources#clang#libclang_path = '/usr/lib64/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib64/clang'

" Tagbar Config
"let g:tagbar_autofocus = 1

" Syntastic Config TODO replace
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
"let g:syntastic_rust_checkers = ['rustc']
"let g:syntastic_python_checkers = ['pylint', 'python']
"let g:syntastic_perl_checkers = ['perl']
"let g:syntastic_cpp_checkers = ['cppcheck', 'clang_check', 'gcc']
"let g:syntastic_c_checkers = ['clang_check', 'gcc']
"let g:syntastic_enable_perl_checker = 1


" -----------------------------------------------------------------------------

" File Specific Config
" --------------------

" TODO change on filetype
set expandtab     " et -- Change tabs into spaces
set shiftwidth=4  " sw
set softtabstop=4 " sts
set textwidth=80  " tw --
set tabstop=8     " ts

set showbreak=+++               " Wrap broken line prefix
set breakindent
set nolist                      " list disables linebreak
set wrapmargin=0                " Set wrap margin to zero
set shiftround
set expandtab
set smarttab
set linebreak                   " breaks lines at words (requires line wrap)
set autoindent                  " enable auto indentation
set cindent
set formatoptions+=t
set formatoptions-=l

" Text Files
augroup text "{{{
    autocmd!
    autocmd FileType text,tex,markdown,org setlocal wrap linebreak nolist
    autocmd FileType text,tex,markdown,org setlocal textwidth=0 wrapmargin=0
    autocmd FileType text,tex,markdown,org setlocal spell! spelllang=en_gb
    " TODO try to disable column highlighting after 80
augroup END  "}}}

" Binary Files
" Change Vim into a hex editor
augroup binary "{{{
  au!
  au BufReadPre   *.bin let &bin=1
  au BufReadPost  *.bin if &bin | %!xxd
  au BufReadPost  *.bin set ft=xxd | endif
  au BufWritePre  *.bin if &bin | %!xxd -r
  au BufWritePre  *.bin endif
  au BufWritePost *.bin if &bin | %!xxd
  au BufWritePost *.bin set nomod | endif
augroup END "}}}

" GPG Encrypted Files
" Transparent editing of gpg encrypted files. By Wouter Hanegraaff.
augroup encrypted "{{{
  au!

  " First make sure nothing is written to ~/.viminfo while editing
  " an encrypted file.
  autocmd BufReadPre,FileReadPre *.gpg,*.asc,*.pgp set viminfo=
  " We don't want a various options which write unencrypted data to disk
  autocmd BufReadPre,FileReadPre *.gpg,*.asc,*.pgp set noswapfile noundofile nobackup

  " Switch to binary mode to read the encrypted file
  autocmd BufReadPre,FileReadPre *.gpg,*.asc,*.pgp set bin
  autocmd BufReadPre,FileReadPre *.gpg,*.asc,*.pgp let ch_save = &ch|set ch=2
  " (If you use tcsh, you may need to alter this line.)
  autocmd BufReadPost,FileReadPost *.gpg,*.asc,*.pgp '[,']!gpg --decrypt 2> /dev/null

  " Switch to normal mode for editing
  autocmd BufReadPost,FileReadPost *.gpg,*.asc,*.pgp set nobin
  autocmd BufReadPost,FileReadPost *.gpg,*.asc,*.pgp let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost *.gpg,*.asc,*.pgp execute ":doautocmd BufReadPost " . expand("%:r")

  " Convert all text to encrypted text before writing
  " (If you use tcsh, you may need to alter this line.)
  autocmd BufWritePre,FileWritePre *.gpg,*.asc,*.pgp '[,']!gpg --default-recipient-self -ae 2>/dev/null
  " Undo the encryption so we are back in the normal text, directly
  " after the file has been written.
  autocmd BufWritePost,FileWritePost *.gpg,*.asc,*.pgp u
augroup END "}}}

