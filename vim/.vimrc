" =============================================================
" Description:  Vim Configuration File
" File:         ~/.vimrc
" =============================================================

" Essentials
filetype plugin indent on
if !exists('g:syntax_on')
    syntax enable
endif
set hidden
set autoread
set mouse=a
set backspace=indent,eol,start
set encoding=utf-8
set spelllang=en_gb

" Styling
set showcmd
set ruler
set belloff=all
set showmatch

" Searching
set hlsearch
set incsearch

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

" Set <Leader> and <LocalLeader> prefixes
let mapleader = "\\"
let maplocalleader = "\\\\"

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

" Line wrap config (soft wrap code, hard wrap comments, strings and text)
let &showbreak='+++ '
set wrap
set linebreak
set breakindent

let g:netrw_banner = 0
packadd matchit
packadd commentary
packadd lion

" Set colour scheme
colorscheme envy
nmap <leader>h <Plug>HighlightGroup
set colorcolumn=+1

com! -nargs=+ GitGrep setl gp=git\ grep\ -n|gr <args>|setl gp&
com! -nargs=* -complete=file -bar TODOs setl gp=todos|gr <args>|setl gp&
com! -nargs=0 -bar Helptags for p in glob('~/.vim/pack/*/*/*/doc',1,1)|exe 'helpt '.p|endfo
com! -nargs=0 -bar Scratch enew|setl bh=hide bt=nofile noswf

" TODO set up for JavaScript and TypeScript development (and qf list)

augroup filetypes
    autocmd!
    autocmd FileType c,make,go,gitconfig,help setlocal noet sts=8 sw=8 ts=8
    autocmd FileType lisp,json,ruby,html,css setlocal et sts=2 sw=2 ts=8
    autocmd FileType perl,sh,python,haskell,javascript setlocal tw=79
    autocmd FileType gitcommit setlocal spell
    autocmd FileType tex setlocal mp=latexmk\ -pdf\ %
    autocmd FileType perl compiler perl
    autocmd FileType sh setlocal mp=shellcheck\ -f\ gcc\ %
                \ efm=%f:%l:%c:\ %trror:\ %m,
                \%f:%l:%c:\ %tarning:\ %m,
                \%f:%l:%c:\ note:\ %m,
                \%f:%l:%c:\ %m
    autocmd FileType markdown setlocal commentstring=<!--%s-->
    autocmd FileType lisp setlocal commentstring=;;%s
    autocmd BufRead,BufNewFile *.ts setlocal filetype=javascript
    autocmd BufRead,BufNewFile *.vue setfiletype html
augroup END
