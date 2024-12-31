" ~/.config/nvim/init.vim

filetype plugin indent on
if !exists('g:syntax_on')
    syntax enable
endif

" Essentials
set mouse=a
set spelllang=en_gb
set formatoptions+=jpl1n
set cpoptions+=J
set joinspaces
set showmatch
set nofoldenable
set startofline
set nrformats=bin,hex,unsigned

" Backup, swap and undo
set backup
set undofile
set backupdir-=.
for s:dir in [&backupdir, &directory, &undodir]
    if !isdirectory(s:dir)
        call mkdir(s:dir, 'p')
    endif
endfor
unlet s:dir

" Clipboard
if has('mac')
    set clipboard=unnamed
endif

" Completion
set wildignore+=*/.git/*,*/node_modules/*,tags,.DS_Store,*/.cpcache/*
set path=.,,**
set dictionary=/usr/dict/words,/usr/share/dict/words

" Indentation
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab
set shiftround
set autoindent

" Line wrap
let &showbreak='>>> '
set wrap
set linebreak
set breakindent

" Theming
if &term =~# '256color$' || has('gui')
    set termguicolors
    colorscheme raider
    let &colorcolumn='+'.join(range(1,256),',+')
endif

if &term =~# '^\(screen\|tmux\)'
    " :help xterm-true-color
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

" Match column numbers in 'grepformat'
set grepformat^=%f:%l:%c:%m

" "<Leader>" and "<LocalLeader>" prefixes
let g:mapleader = " "
let g:maplocalleader = "\\"

" Sensible default mappings
nnoremap Q gq

if exists(':menu')
    aunmenu PopUp.-1-
    aunmenu PopUp.How-to\ disable\ mouse
endif

" Create parent directories on buffer write if they don't exist.
function! s:create_parent_dirs()
    let dir = expand("%:p:h")
    if ! isdirectory(dir) && confirm('Create directory "'.dir.'"?', "&Yes\n&No") == 1
        call mkdir(dir, 'p')
    endif
endfunction

function! s:trim_whitespace()
    if ! get(b:, 'no_whitespace_trim')
        let view = winsaveview()
        keeppatterns %s/\s\+$//e
        call winrestview(view)
    endif
endfunction

augroup file_utils
    autocmd!
    autocmd BufWritePre * call s:create_parent_dirs()
    autocmd BufWritePre * call s:trim_whitespace()
    " Don't trim whitespace on diff files as it breaks syntax highlighting.
    autocmd FileType gitcommit,diff let b:no_whitespace_trim = 1
augroup END

augroup cursor_restore
    autocmd!
    " When editing a file, jump to the last known cursor position.
    autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " Reset cursor position to top of file for these patterns.
    autocmd BufReadPost */.git/* normal! gg0
augroup END

augroup filetype_config
    autocmd!

    autocmd FileType c,cpp,go,gitconfig,fstab setlocal noet sts=8 sw=8
    autocmd FileType lisp,clojure,scheme,json,ruby,markdown setlocal et sts=2 sw=2
    autocmd FileType html,css setlocal noet sts=2 sw=2 ts=2
    autocmd FileType perl,sh,python,javascript setlocal tw=79
    autocmd FileType gitcommit setlocal spell
    autocmd FileType lisp,clojure,scheme setlocal commentstring=;;%s cpo-=J
    autocmd FileType robots,crontab,spec,desktop setlocal commentstring=#%s

    autocmd FileType c,cpp setlocal path+=/usr/include
    autocmd FileType tex compiler latexmk
    autocmd FileType sh  compiler shellcheck

    autocmd FileType jsonl setlocal nowrap

    autocmd BufRead,BufNewFile *.prolog,*.pro,*.PRO,*.pg setfiletype prolog

    " Redo <https://cr.yp.to/redo.html> <http://news.dieweltistgarnichtso.net/bin/redo-sh.html>
    autocmd BufRead,BufNewFile *.do setlocal filetype=sh

    autocmd BufRead,BufNewFile TODO,DOING,DONE setlocal ft=markdown

    autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=100
augroup END
