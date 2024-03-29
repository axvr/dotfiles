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
set omnifunc=syntaxcomplete#Complete
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

" "<Leader>" and "<LocalLeader>" prefixes
let g:mapleader = " "
let g:maplocalleader = "\\"

" Sensible default mappings
nnoremap Q gq

augroup cursor_restore
    autocmd!
    " When editing a file, jump to the last known cursor position.
    autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " Reset cursor position to line 1 when writing a Git commit/tag message.
    autocmd BufReadPost COMMIT_EDITMSG,TAG_EDITMSG normal! gg0
augroup END

augroup filetype_config
    autocmd!

    autocmd FileType c,cpp,go,gitconfig,fstab setlocal noet sts=8 sw=8
    autocmd FileType lisp,clojure,scheme,json,ruby setlocal et sts=2 sw=2
    autocmd FileType html,css setlocal noet sts=2 sw=2 ts=2
    autocmd FileType perl,sh,python,javascript setlocal tw=79
    autocmd FileType gitcommit setlocal spell
    autocmd FileType lisp,clojure,scheme setlocal commentstring=;;%s cpo-=J
    autocmd FileType robots,crontab,spec,desktop setlocal commentstring=#%s

    autocmd FileType c,cpp setlocal path+=/usr/include
    autocmd FileType tex compiler latexmk
    autocmd FileType sh  compiler shellcheck

    autocmd FileType jsonl setlocal nowrap

    " Redo <https://cr.yp.to/redo.html> <http://news.dieweltistgarnichtso.net/bin/redo-sh.html>
    autocmd BufRead,BufNewFile *.do setlocal filetype=sh

    autocmd BufRead,BufNewFile TODO,DOING,DONE setlocal ft=markdown et sts=2 sw=2
augroup END
