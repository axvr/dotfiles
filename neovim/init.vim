" Neovim Configuration File (~/.config/nvim/init.vim)
" ===================================================

" Brief help
" ----------
" :PluginList	    - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginClean	    - confirms removal of unused plugins; append `!` to auto-approve removal
" :PluginUpdate	    - update installed plugins
" :source $MYVIMRC  - update to the current version if ~/.vimrc


" Plugin Setup

  " Install Vim-Plug and Plugins
    command! InstallVimPlug !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


  call plug#begin()
  " Input Plugins Below this Line

    Plug 'scrooloose/nerdtree',           { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }   " NERDTree Plugin  <-- :help NERD_tree.txt
    Plug 'xuyuanp/nerdtree-git-plugin',   { 'on': ['NERDTreeToggle', 'NERDTreeFind'] }   " Display Git Diffs in NERDTree
    Plug 'tpope/vim-fugitive'             " Fugitive.Vim Git Wrapper Plugin   <-- :help fugitive
    Plug 'terryma/vim-multiple-cursors'   " Vim Multiple Cursors Plugin       <-- :help vim-multiple-cursors
    Plug 'scrooloose/syntastic'           " Syntastic Syntax Checker Plugin   <-- :help syntastic
    Plug 'airblade/vim-gitgutter'         " Show a Git Diff in the 'Gutter'   <-- :help GitGutter
    Plug 'majutsushi/tagbar'              " Display Tags of a File Easily     <-- :help tagbar
    Plug 'ctrlpvim/ctrlp.vim'             " CtrlP Plugin                      <-- :help ctrlp.txt
    Plug 'vim-airline/vim-airline'        " Airline Theme Plugin              <-- :help Airline
    Plug 'vim-airline/vim-airline-themes' " Airline Theme Packages
    Plug 'rafi/awesome-vim-colorschemes'  " Colour Schemes for Vim and Nvim
    Plug 'rust-lang/rust.vim'             " Rust Syntax Highlighting

  " Input Plugins Above this Line
  call plug#end()


" Basic Config

  set number relativenumber       " Show the line numbers
  set linebreak                   " breaks lines at words (requires line wrap)
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
  set omnifunc=syntaxcomplete#Complete
  set path+=**
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
  autocmd BufWritePre * %s/\s\+$//e " Remove trailing whitespace

  " Vim Colour Scheme
    colorscheme space-vim-dark
    if (has("termguicolors"))
      set termguicolors
    endif

  " Vim Spell Check
  noremap <F7> :setlocal spell! spelllang=en_us<CR>

  " Make a tags file
  command! MakeTags !ctags -R .

  " Auto Close Parens and Quotes
  " TODO make more inteligent
    "ino " ""<left>
    "ino ' ''<left>
    "ino ( ()<left>
    "ino [ []<left>
    "ino { {}<left>
    ino {<CR> {<CR>}<ESC>O

  " Vim Hard Mode
    "noremap  <Up> ""
    "noremap! <Up> <Esc>
    "noremap  <Down> ""
    "noremap! <Down> <Esc>
    "noremap  <Left> ""
    "noremap! <Left> <Esc>
    "noremap  <Right> ""
    "noremap! <Right> <Esc>
    set mouse=a

  " Vim Omnicomplete
    au BufNewFile,BufRead,BufEnter *.cpp,*.hpp set omnifunc=omni#cpp#complete#Main
    set completeopt=longest,menuone
    inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
      \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
    inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
      \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

  " Vim Tabs Config
    nnoremap <F5> :tabp<CR>
    nnoremap <F6> :tabn<CR>

  " Vim Folding - Does not work fully in Neovim
    augroup vim_folding
      inoremap <F9> <C-O>za
      nnoremap <F9> za
      onoremap <F9> <C-C>za
      vnoremap <F9> zf
      au BufReadPre * setlocal foldmethod=indent
      au BufWinEnter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
    augroup END

  " GVim Config
    if has('gui_running')	" Setup GVim to usable configurations
      set guifont=Monospace\ 11	" set font Monospace 11
    endif


" Plugin Config

  " Airline Config
    let g:airline#extensions#tagbar#enabled = 1
    let g:airline#extensions#syntastic#enabled = 1
    let g:airline_theme='lucius' " Set Airline theme

  " NERDTree Config
    let g:NERDTreeChDirMode=2
    let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
    let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
    let g:NERDTreeShowBookmarks=1
    let g:nerdtree_tabs_focus_on_files=1
    let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
    let g:NERDTreeWinSize = 25
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
    nnoremap <silent> <F2> :NERDTreeFind<CR>
    noremap <F3> :NERDTreeToggle<CR>

  " Syntastic Config
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    let g:syntastic_rust_checkers = ['rustc']
    let g:syntastic_python_checkers = ['pylint', 'python']
    let g:syntastic_perl_checkers = ['perl']
    let g:syntastic_cpp_checkers = ['cppcheck', 'clang_check', 'gcc']
    let g:syntastic_c_checkers = ['clang_check', 'gcc']
    let g:syntastic_enable_perl_checker = 1

  " CtrlP Config
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
    let g:ctrlp_prompt_mappings = {
      \ 'AcceptSelection("e")': ['<c-t>'],
      \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
      \ }

  " Tagbar Config
    nmap <silent> <F4> :TagbarToggle<CR>
    let g:tagbar_autofocus = 1


" Syntax & File Type Config

  " Mozilla Syntax Vim Config
  " TODO Extend and enhance this.
  if (&filetype == 'python')
    set ts=8 sts=4 et sw=4 tw=80
  else
    set ts=8 sts=2 et sw=2 tw=80
  endif

  " Binary Files
  " Change vim into a hex editor
    augroup binary
      au!
      au BufReadPre   *.bin  let &bin=1
      au BufReadPost  *.bin  if &bin | %!xxd
      au BufReadPost  *.bin  set ft=xxd | endif
      au BufWritePre  *.bin  if &bin | %!xxd -r
      au BufWritePre  *.bin  endif
      au BufWritePost *.bin  if &bin | %!xxd
      au BufWritePost *.bin  set nomod | endif
    augroup END


