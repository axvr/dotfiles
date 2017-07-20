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


" -----------------------------------------------------------------------------
" TODO line & tab length
" TODO Auto-complete for all languages wanted
" TODO change configuration based upon file type
" TODO custom Vim + airline theme (change TODO colour)
" TODO replace syntastic
" TODO improve Vim buffers (similar to SM)
" TODO improve Vim vimrc
" TODO try to use same tools (e.g. linters) as SM
" TODO spelling fix
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

" Install Vim-Plug & Plugins
command! SetupNvim !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim
      \ --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

call plug#begin()
  " Input Plugins Below this Line {{{

  " File viewers and switchers
  Plug 'ctrlpvim/ctrlp.vim',            " CtrlP Fuzzy Finder                <-- :help ctrlp.txt
  Plug 'scrooloose/nerdtree',           { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }   " NERDTree Plugin  <-- :help NERD_tree.txt
  Plug 'Xuyuanp/nerdtree-git-plugin',   { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }   " Display Git Diffs in NERDTree

  " Auto-complete
  Plug 'jiangmiao/auto-pairs'   " Smart brackets and quotes
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'Shougo/neco-vim', { 'for': 'vim' }        " VimL completion
  Plug 'zchee/deoplete-clang'   " Clang completion engine
  Plug 'Shougo/neco-syntax'     " Many languages simple completion engine
  Plug 'poppyschmo/deoplete-latex', { 'for': 'tex' } " Experimental LaTeX auto-completion engine
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
  Plug 'majutsushi/tagbar'             " Display Tags of a File Easily     <-- :help tagbar
  Plug 'jceb/vim-orgmode' " :help orgguide

  " Git integration
  Plug 'tpope/vim-fugitive'             " Fugitive.Vim Git Wrapper Plugin   <-- :help fugitive
  Plug 'airblade/vim-gitgutter'         " Show a Git Diff in the 'Gutter'   <-- :help GitGutter
  Plug 'rhysd/committia.vim'            " More Pleasant Editing on Commit Message

  " Syntax highlighting packs
  Plug 'rust-lang/rust.vim'             " Rust Syntax Highlighting
  Plug 'tomlion/vim-solidity'           " Solidity Syntax Highlighting
  Plug 'lervag/vimtex', { 'for': 'tex' } " Install latexmk

  " Colourschemes and themes
  Plug 'vim-airline/vim-airline'        " Airline Theme Plugin              <-- :help Airline
  Plug 'vim-airline/vim-airline-themes' " Airline Theme Packages
  Plug 'rafi/awesome-vim-colorschemes'  " Colour Schemes for Vim

  " Input Plugins Below this line }}}
call plug#end()


" -----------------------------------------------------------------------------

" Basic Configuration
" -------------------

set number relativenumber       " Show the line numbers
set linebreak                   " breaks lines at words
set showbreak=+++               " Wrap broken line prefix
set showmatch                   " Highlight matching brackets
set ignorecase                  " Ignore case in searches
set smartcase                   " enables smart case mode
set confirm                     " confirmation prompts
set undolevels=1000             " number of undo levels
set wrap                        " wrap visually
set nolist                      " list disables linebreak
set nomodeline
set expandtab                   " Changes tabs into spaces
set wrapmargin=0                " Set wrap margin to zero
set cursorline                  " Highlight current line
set omnifunc=syntaxcomplete#Complete
set path+=**
set mouse=a
set foldmethod=marker
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
autocmd BufWritePre * %s/\s\+$//e " Remove trailing whitespace

" Vim & GVim styling
set background=dark
colorscheme hybrid_material
if (has("termguicolors"))
  set termguicolors
endif
if has('gui_running')
  set guifont=Monospace\ 11
endif

" Vim Omnicomplete
autocmd BufNewFile,BufRead,BufEnter *.cpp,*.hpp
                        \ set omnifunc=omni#cpp#complete#Main
set completeopt=longest,menuone
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
                        \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
                        \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'


" -----------------------------------------------------------------------------

" Set Keymaps & Commands
" ----------------------

" Set Leader key
nnoremap <Space> \
let mapleader = "\\"

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
autocmd FileType c,h,cpp,hpp,cc,objc
      \ nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,h,cpp,hpp,cc,objc
      \ vnoremap <buffer><Leader>cf :ClangFormat<CR>


" -----------------------------------------------------------------------------

" Plugin Configurattion
" ---------------------

" Airline Config
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_theme='lucius' " Set Airline theme

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

" Clang Format Config
" TODO Java, JavaScript, Obj-C, C
let g:clang_format#code_style = 'google'

" Deoplete Config
let g:deoplete#enable_at_startup = 1
" Clang - find locations: https://github.com/zchee/deoplete-clang
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

" TODO replace this
"if (&filetype == 'python')
"  set ts=8 sts=4 et sw=4 tw=80
"else
"  set ts=8 sts=2 et sw=2 tw=80
"endif

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

