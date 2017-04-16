" NeoVim Configuration File (~/.config/nvim/init.vim)
" ===================================================

" $ sudo dnf -y install neovim python2-neovim python3-neovim
" $ sudo dnf upgrade && sudo dnf install pylint rust python perl ctags cppcheck
" $ sudo dnf install automake gcc gcc-c++ kernel-devel cmake python-devel python3-devel
" " perl-B-Lint ShellCheck
"
" Brief help
" ----------
" :PluginList	    - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean	    - confirms removal of unused plugins; append `!` to auto-approve removal
" :PluginUpdate	    - update installed plugins
" :source $MYVIMRC  - update to the current version if ~/.vimrc

" Plugin Setup

  " Auto Install Vivid and Plugins
    let vivid_checkfile=expand('~/.vim/bundle/Vivid-Legacy.vim/test-files/checkfile.txt')
    if (!filereadable(vivid_checkfile))
      echo "Installing Vivid-Legacy.vim & plugins"
      silent !git clone https://github.com/VividVim/Vivid-Legacy.vim.git ~/.vim/bundle/Vivid-Legacy.vim
      :source $MYVIMRC
      :PluginInstall
      :source $MYVIMRC
      :q
    endif

  set nocompatible
  filetype off
  set rtp+=~/.vim/bundle/Vivid-Legacy.vim
  call vivid#open()
  " Input Plugins Below this Line

    Plugin 'VividVim/Vivid-Legacy.vim'		" let Vivid manage Vivid	    <-- :help vivid
    Plugin 'rust-lang/rust.vim'		        " Rust Syntax Highlighting
    Plugin 'kchmck/vim-coffee-script'	        " CoffeeScript Syntax Highlighting
    Plugin 'vim-airline/vim-airline'	        " Airline Theme Plugin		    <-- :help Airline
    Plugin 'vim-airline/vim-airline-themes'	" Airline Theme Packages
    Plugin 'tpope/vim-fugitive'		        " Fugitive.Vim Git Wrapper Plugin   <-- :help fugitive
    Plugin 'terryma/vim-multiple-cursors'       " Vim Multiple Cursors Plugin	    <-- :help vim-multiple-cursors
    Plugin 'scrooloose/syntastic'	        " Syntastic Syntax Checker Plugin   <-- :help syntastic
    Plugin 'airblade/vim-gitgutter'		" Show a Git Diff in the 'Gutter'   <-- :help GitGutter
    Plugin 'majutsushi/tagbar'		        " Display Tags of a File Easily	    <-- :help tagbar
    Plugin 'scrooloose/nerdtree'		" NERDTree Plugin		    <-- :help NERD_tree.txt
    Plugin 'jistr/vim-nerdtree-tabs'	        " NERDTree Tabs Plugin              <-- :help vim-nerdtree-tabs
    Plugin 'xuyuanp/nerdtree-git-plugin'	" Display Git Diffs in NERDTree
    Plugin 'ctrlpvim/ctrlp.vim'		        " CtrlP Plugin			    <-- :help ctrlp.txt
    Plugin 'mhartington/oceanic-next'

  " Input Plugins Above this Line
  call vivid#close()
  filetype indent plugin on



" Basic Config

  set number                  " Show the line numbers
  set relativenumber
  set linebreak               " breaks lines at words (requires line wrap)
  set showbreak=+++           " Wrap broken line prefix
  set showmatch               " Highlight matching brackets
  set visualbell              " Use a visual error bell
  set ignorecase              " Ignore case in searches
  set smartcase               " enables smart case mode
  set confirm                 " confirmation prompts
  set ruler                   " show row and col ruler info
  set undolevels=1000         " number of undo levels
  set wrap                    " wrap visually
  set nolist                  " list disables linebreak
  set nomodeline
  set expandtab               " Changes tabs into spaces
  set wrapmargin=0            " Set wrap margin to zero
  set visualbell t_vb=        " Disable sound alerts
  set omnifunc=syntaxcomplete#Complete
  set path+=**
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
  autocmd BufWritePre * %s/\s\+$//e " Remove trailing whitespace
  " Vim Spell Check
  noremap <F7> :setlocal spell! spelllang=en_us<CR>
  " Make a tags file
  command! MakeTags !ctags -R .

  " Vim Folding - Does not work fully
    augroup vim_folding
      inoremap <F9> <C-O>za
      nnoremap <F9> za
      onoremap <F9> <C-C>za
      vnoremap <F9> zf
      au BufReadPre * setlocal foldmethod=indent
      au BufWinEnter * if &fdm == 'indent' | setlocal foldmethod=manual | endif
    augroup END

  " Auto Close Parens and Quotes
    "ino " ""<left>
    "ino ' ''<left>
    "ino ( ()<left>
    "ino [ []<left>
    "ino { {}<left>
    "ino {<CR> {<CR>}<ESC>O

  " GVim Config
    if has('gui_running')	" Setup GVim to usable configurations
      colo desert		" colour scheme = desert
      set guifont=Monospace\ 11	" set font Monospace 11
    endif

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



" Plugin Config

  " Airline Config
    let g:airline#extensions#tagbar#enabled = 1
    let g:airline#extensions#syntastic#enabled = 1
    let g:airline_theme='lucius' " Set Airline theme (favs: 'lucius' 'luna' 'raven')

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
  if (&filetype == 'python')
    set ts=8 sts=4 et sw=4 tw=80
  else
    set ts=8 sts=2 et sw=2 tw=80
  endif

  " Binary & ASCII Files
  " Change vim into a hex editor
    augroup binary
      au!
      au BufReadPre   *.bin,*.asc let &bin=1
      au BufReadPost  *.bin,*.asc if &bin | %!xxd
      au BufReadPost  *.bin,*.asc set ft=xxd | endif
      au BufWritePre  *.bin,*.asc if &bin | %!xxd -r
      au BufWritePre  *.bin,*.asc endif
      au BufWritePost *.bin,*.asc if &bin | %!xxd
      au BufWritePost *.bin,*.asc set nomod | endif
    augroup END

  " GPG Encrypted Files
  " Transparent editing of gpg encrypted files. By Wouter Hanegraaff.
    augroup encrypted
      au!

      " First make sure nothing is written to ~/.viminfo while editing
      " an encrypted file.
      autocmd BufReadPre,FileReadPre *.gpg set viminfo=
      " We don't want a various options which write unencrypted data to disk
      autocmd BufReadPre,FileReadPre *.gpg set noswapfile noundofile nobackup

      " Switch to binary mode to read the encrypted file
      autocmd BufReadPre,FileReadPre *.gpg set bin
      autocmd BufReadPre,FileReadPre *.gpg let ch_save = &ch|set ch=2
      " (If you use tcsh, you may need to alter this line.)
      autocmd BufReadPost,FileReadPost *.gpg '[,']!gpg --decrypt 2> /dev/null

      " Switch to normal mode for editing
      autocmd BufReadPost,FileReadPost *.gpg set nobin
      autocmd BufReadPost,FileReadPost *.gpg let &ch = ch_save|unlet ch_save
      autocmd BufReadPost,FileReadPost *.gpg execute ":doautocmd BufReadPost " . expand("%:r")

      " Convert all text to encrypted text before writing
      " (If you use tcsh, you may need to alter this line.)
      autocmd BufWritePre,FileWritePre *.gpg '[,']!gpg --default-recipient-self -ae 2>/dev/null
      " Undo the encryption so we are back in the normal text, directly
      " after the file has been written.
      autocmd BufWritePost,FileWritePost *.gpg u
    augroup END



" Colour Scheme

  " For Neovim 0.1.3 and 0.1.4
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1

  " Or if you have Neovim >= 0.1.5
  if (has("termguicolors"))
   set termguicolors
  endif

  " Theme
  syntax enable
  colorscheme OceanicNext


