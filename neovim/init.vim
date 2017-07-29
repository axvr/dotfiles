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


set nocompatible

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
" TODO indentation
" TODO Auto-complete for all languages wanted
" TODO improve Vim buffers (similar to SM)
" TODO improve Vim vimrc
" TODO spelling fix / correction keymap (maybe list corrections)
" TODO tidy up this document
" TODO custom Vim + airline + lightline theme (change TODO colour)
"
" Languages still to optimise for
" * C
" * Rust
" * Python
" * Java
" * JavaScript
" * HTML
" * CSS
" -----------------------------------------------------------------------------

" Plugin Setup
" ------------

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
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
Plug 'scrooloose/nerdtree',           { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }    " NERDTree Plugin  <-- :help NERD_tree.txt
Plug 'Xuyuanp/nerdtree-git-plugin',   { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }    " Display Git Diffs in NERDTree

" Auto-complete
Plug 'jiangmiao/auto-pairs'                                               " Smart brackets and quotes
Plug 'Shougo/deoplete.nvim',          { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neco-vim',               { 'for': 'vim' }                    " VimL completion
Plug 'zchee/deoplete-clang'                                               " Clang completion engine
Plug 'Shougo/neco-syntax'                                                 " Many languages simple completion engine
Plug 'poppyschmo/deoplete-latex',     { 'for': 'tex' }                    " Experimental LaTeX auto-completion engine
" TODO Python completion engine 'zchee/deoplete-jedi'
" TODO Rust completion engine 'sebastianmarkow/deoplete-rust'
" TODO others

" Syntax checking (linting)
Plug 'neomake/neomake'

" Code formatting
Plug 'rhysd/vim-clang-format', { 'on': ['ClangFormat', 'ClangFormatAutoToggle', 'ClangFormatAutoEnable'] }  " Format files using Clang

" Vim enhancements
Plug 'terryma/vim-multiple-cursors'   " Vim Multiple Cursors Plugin       <-- :help vim-multiple-cursors
Plug 'rhysd/clever-f.vim'
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }              " Display Tags of a File Easily     <-- :help tagbar
Plug 'jceb/vim-orgmode', { 'for': 'org' }               " :help orgguide
Plug 'tpope/vim-speeddating', { 'for': 'org' }          " Increment dates

" Git integration
Plug 'tpope/vim-fugitive'             " Fugitive.Vim Git Wrapper Plugin   <-- :help fugitive
Plug 'airblade/vim-gitgutter'         " Show a Git Diff in the 'Gutter'   <-- :help GitGutter
Plug 'rhysd/committia.vim'            " More Pleasant Editing on Commit Message

" Syntax highlighting packs
Plug 'rust-lang/rust.vim'             " Rust Syntax Highlighting
Plug 'tomlion/vim-solidity'           " Solidity Syntax Highlighting
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'octol/vim-cpp-enhanced-highlight'

" Colourschemes and themes
Plug 'itchyny/lightline.vim'          " Lightline Theme Plugin
"Plug 'vim-airline/vim-airline'        " Airline Theme Plugin  <-- :help Airline
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

" Essentials
filetype plugin indent on
syntax enable

" Searching
set ignorecase                  " Ignore case in searches
set smartcase                   " Enables smart case mode
set hlsearch                    " Highlight all search results
set incsearch                   " Searches for strings incrementaly
set wrapscan                    " Wrap back to the start of the file

" Mode line
set modeline
set modelines=5

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

" TODO set backup dir & manage swap files
set autoread
set showcmd
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
let &colorcolumn=join(range(81,335), ",")
set visualbell t_vb=            " Disable sound & visual alerts

set background=dark
colorscheme tender
"hi Normal guibg=NONE ctermbg=NONE
if (has("termguicolors"))
    set termguicolors
endif
if (has('gui_running'))
    set t_Co=256
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
nnoremap <leader>bb :<C-u>buffers<CR>:<C-u>buffer<Space>
nnoremap <leader>qq :<C-u>qa<CR>
nnoremap <leader>gs :<C-u>Gstatus<CR>
nnoremap <leader>gc :<C-u>Gcommit<CR>
nnoremap <leader>gd :<C-u>Gdiff<CR>
nnoremap <leader>ge :<C-u>Gedit<CR>
nnoremap <leader>gm :<C-u>Gmove<CR>
nnoremap <leader>gr :<C-u>Gdelete<CR>
nnoremap <leader>gb :<C-u>Gblame<CR>
nnoremap <leader>p  :<C-u>CtrlP<CR>
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
autocmd FileType c,h,cpp,hpp,cc,objc
            \ nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,h,cpp,hpp,cc,objc
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

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX files.
function! AppendModeline()
    let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
                \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
    let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
    call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :<C-u>call AppendModeline()<CR>


" -----------------------------------------------------------------------------

" Plugin Configurattion
" ---------------------

" Airline Config
"let g:airline_theme='lucius' " Set Airline theme

" Lightline Config
let g:lightline = {
            \ 'colorscheme': 'tender',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'gitbranch', 'readonly', 'relativepath', 'modified' ] ]
            \ },
            \ 'component_function': {
            \   'gitbranch': 'fugitive#head',
            \ },
            \ }

" CtrlP Config
" -----------------------------------------------------------------------------
"     Run :CtrlP or :CtrlP [starting-directory] to invoke CtrlP in find file mode.
"     Run :CtrlPBuffer or :CtrlPMRU to start CtrlP in find buffer or find MRU file mode.
"     Run :CtrlPMixed to search in Files, Buffers and MRU files at the same time.
"
" Check :help ctrlp-commands and :help ctrlp-extensions for other commands.
" Run :help ctrlp-mappings or submit ? in CtrlP for more mapping help.
"
" Once CtrlP is open:
"     Press <F5> to purge the cache for the current directory to get new files, remove deleted files and apply new ignore options.
"     Press <c-f> and <c-b> to cycle between modes.
"     Press <c-d> to switch to filename search instead of full path.
"     Press <c-r> to switch to regexp mode.
"     Use <c-j>, <c-k> or the arrow keys to navigate the result list.
"     Use <c-t> or <c-v>, <c-x> to open the selected entry in a new tab or in a new split.
"     Use <c-n>, <c-p> to select the next/previous string in the prompt's history.
"     Use <c-y> to create a new file and its parent directories.
"     Use <c-z> to mark/unmark multiple files and <c-o> to open them.
"
"     Submit two or more dots .. to go up the directory tree by one or multiple levels.
"     End the input with a colon : followed by a command to execute it after opening the file:
"         Use :25 to jump to line 25.
"         Use :/any\:\ string to jump to the first instance of any: string.
"         Use :+setfiletype\ myfiletype|25 to set the filetype to myfiletype then jump to line 25.
"         Use :diffthis when opening files marked by <c-z> to run :diffthis on the first 4 files.
" -----------------------------------------------------------------------------
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

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
let g:clang_format#detect_style_file = 1

" Deoplete Config
let g:deoplete#enable_at_startup = 1
" Deoplete-Clang - find locations: https://github.com/zchee/deoplete-clang
let g:deoplete#sources#clang#libclang_path = '/usr/lib64/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib64/clang'

" Tagbar Config
"let g:tagbar_autofocus = 1

" Neomake Config
autocmd! BufWritePost * Neomake
let g:neomake_c_enabled_makers = ['clang', 'gcc'] " TODO C test and configure
let g:neomake_cpp_enabled_makers = ['clangtidy', 'cppcheck']
let g:neomake_cpp_clangtidy_maker = {
            \ 'exe': 'clang-tidy',
            \ 'args': ['-checks=*'],
            \ }
let g:neomake_python_enabled_makers = ['pylint', 'python'] " TODO Python3
let g:neomake_perl_enabled_makers = ['perl']
let g:neomake_rust_enabled_makers = ['rustc']
let g:neomake_sh_enabled_makers = ['shellcheck']


" -----------------------------------------------------------------------------

" File Specific Config
" --------------------

" TODO change on filetype
set expandtab     " et -- Change tabs into spaces
set shiftwidth=4  " sw
set softtabstop=4 " sts
set textwidth=80  " tw --
set tabstop=8     " ts

let &showbreak='>>> '           " Wrap broken line & prefix
set nolist                      " list disables linebreak
set wrapmargin=0                " Set wrap margin to zero
set shiftround
set smarttab
set linebreak                   " breaks lines at words (requires line wrap)
set autoindent                  " enable auto indentation
set cindent                     " C style indentation
set formatoptions+=t
set formatoptions-=l

" Text Files (text, tex, markdown, org, gitcommit, diff)
augroup text "{{{
    autocmd!
    autocmd FileType text,tex,markdown,org,gitcommit setlocal wrap linebreak nolist
    autocmd FileType text,tex,markdown,org,gitcommit setlocal textwidth=0 wrapmargin=0
    autocmd FileType text,tex,markdown,org,gitcommit setlocal spell spelllang=en_gb
    autocmd FileType text,tex,markdown,gitcommit,diff setlocal nofoldenable
    autocmd FileType org setlocal foldenable

    if exists('+breakindent')
        autocmd FileType text,tex,markdown,org,gitcommit,diff setlocal breakindent
    endif

    autocmd FileType text,tex,markdown,org,gitcommit let &l:colorcolumn=0
    autocmd FileType diff let &colorcolumn=join(range(82,335), ",")
    autocmd FileType text,tex,markdown,org,diff setlocal number norelativenumber
    autocmd FileType gitcommit setlocal nonumber norelativenumber
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


" vim: set ts=8 sw=4 tw=80 et :
