" Vim Configuration File (~/.vimrc)
" =================================

" Brief Help:
"   :PluginInstall      - Install new plugins
"   :PluginUpgrade      - Update installed plugins
"   :PluginEnable       - Enable a named plugin
"   :PluginClean        - Remove unused plugins
"   :mks[ession] path   - Create a session
"   :so[urce] path      - Load a session or Vim Script

" Dependencies:
"   LaTeX:  latexmk & pdflatex
"   Perl:   perl
"   Shell:  shellcheck
"   Clangs: clang-tidy (-checks=* ???), clang, gcc, gcc-c++ (g++)
"   Python: pylint
"   Build:  make, cmake, etc
"   Tools:  ctags


" Initial Config

" Make sure the file is readable
if !1 | finish | endif

" Encoding
scriptencoding utf-8
set encoding=utf-8
if &modifiable != 0
    set fileencoding=utf-8
endif

" Simplify loading of Vim config files
function! s:loadConfig(file) abort
    let l:file = expand(a:file)
    if filereadable(l:file)
        execute 'source ' . l:file
    endif
endfunction

" Basic Configuration {{{

" Essentials
filetype plugin indent on
if !exists('g:syntax_on')
    syntax enable
endif
set hidden confirm
set splitright splitbelow
set autoread
set fileformats=unix,mac,dos
set mouse=a
set backspace=indent,eol,start
set spelllang=en_gb
set history=200
set lazyredraw
set showmatch
set foldenable foldmethod=marker
set modeline modelines=5
set clipboard^=unnamedplus,unnamed

" Searching
set ignorecase      " Ignore case in searches
set smartcase       " Enables smart case mode
set hlsearch        " Highlight all search results
set incsearch       " Searches for strings incrementaly
set wrapscan        " Wrap back to the start of the file

" Backup, Swap & Undo files  {{{
let s:dirs = ['', '', '']
if has('nvim')
    let s:dirs[0] = expand($HOME . '/.config/nvim/backup')
    let s:dirs[1] = expand($HOME . '/.config/nvim/swap')
    let s:dirs[2] = expand($HOME . '/.config/nvim/undo')
else
    let s:dirs[0] = expand($HOME . '/.vim/backup')
    let s:dirs[1] = expand($HOME . '/.vim/swap')
    let s:dirs[2] = expand($HOME . '/.vim/undo')
    set viminfo+=n~/.vim/viminfo
endif
for s:dir in s:dirs
    if !isdirectory(s:dir)
        call mkdir(s:dir, 'p')
    endif
endfor
let &backupdir = s:dirs[0]
set backup
let &directory = s:dirs[1]
if has('persistent_undo')
    let &undodir = s:dirs[2]
    set undofile
else
    set undolevels=1000
endif
unlet s:dirs " }}}

" Vim Omnicomplete, Ins-complete & Wild menu
set omnifunc=syntaxcomplete#Complete
set wildmenu
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
set path+=**

" }}}

" Plugin Config
call <SID>loadConfig('$HOME/.vim/config/plugins.vim')

" Styling
call <SID>loadConfig('$HOME/.vim/config/styling.vim')

" Set Keymaps & Commands {{{

let mapleader = "\<space>"
inoremap jk <ESC>
" Git keybindings
nnoremap <leader>gs :<C-u>Gstatus<CR>
nnoremap <leader>gc :<C-u>Gcommit<CR>
nnoremap <leader>gd :<C-u>Gdiff<CR>
nnoremap <leader>gb :<C-u>Gblame<CR>
nnoremap <leader>ga :<C-u>Gwrite<CR>
nnoremap <leader>gg :<C-u>Git<space>
" Spell check toggle
nnoremap <F7> :<C-u>setlocal spell!<CR>
nnoremap <leader>ss :<C-u>setlocal spell!<CR>
" Make tags file using ctags
command! -nargs=0 MakeTags !ctags -R .
nnoremap <silent> <leader>mt :<C-u>!ctags -R .<CR>

" Remove trailing whitespace {{{
function! s:trim(bang) abort
    if a:bang || (!&binary && &filetype != 'diff')
        normal! mz
        normal! Hmy
        %s/\m\C\s\+$//e
        normal! 'yz<CR>
        normal! `z
    else | echoerr 'Warning! Not reccommended to trim whitespace in this file.'
    endif
endfunction
command! -nargs=0 -bar -bang Trim call <SID>trim('!' == '<bang>') " }}}

" Show Highlighting group for current word
function! s:syn_stack()
    if !exists('*synstack') | return | endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction
nnoremap <leader>hg :call <SID>syn_stack()<CR>

" TODO set up local leader?

" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX files.
function! AppendModeline()
    let l:modeline = printf(' vim: set ts=%d sw=%d tw=%d %set ft=%s fdm=%s ' .
                \ 'fmr=%s :',
                \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no',
                \ &filetype, &foldmethod, &foldmarker)
    let l:modeline = substitute(&commentstring, '%s', l:modeline, '')
    call append(line('$'), l:modeline)
endfunction
nnoremap <Leader>ml :<C-u>call AppendModeline()<CR>

" Allow quick changing of termguicolors
nnoremap <leader>tc :<C-u>set termguicolors!<CR>

" }}}

" File Specific Config

" Indentation Config (Spaces > Tabs)
set tabstop=8       " ts  -- Don't change this value
set softtabstop=4   " sts -- Number of spaces = to a tab
set shiftwidth=4    " sw  -- Number of sapces used for indenting
set expandtab       " et  -- Change tabs into spaces
set shiftround      " sr  -- Round indent to multiple of sw
set autoindent      " ai  -- Enable unintrusive auto-indentation

" Line wrap config (Use soft wrap, and manually hard wrap text)
let &showbreak='>>> '   " Prefix for soft wrapped line
set textwidth=80        " Maximum width of text
set wrapmargin=0        " Set wrap margin on right of screen
set wrap                " Enable line wrap
set linebreak           " Breaks lines at words
set nolist              " List disables linebreak
if exists('+breakindent')
    set breakindent
endif


let g:tex_flavor = "latex"


" vim: set ts=8 sw=4 tw=80 et ft=vim fdm=marker fmr={{{,}}} :
