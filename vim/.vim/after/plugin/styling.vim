" =============================================================
" Description:  Configure Styling and Colours for Vim & GVim
" File:         ~/.vim/after/plugin/styling.vim
" =============================================================

set number relativenumber
set showcmd
set ruler
" TODO redo the ruler format, add more useful information
set rulerformat=%.20(%=%<%(%{&filetype==''?'':'\ '.&ft.'\ '}%)%(\ %P\ \ %2c%)%)
set cursorline          " Highlight current line
let &colorcolumn='+'.join(range(1,256), ',+')
set visualbell t_vb=    " Disable sound & visual alerts
set laststatus=2        " Always display statusline

if &term =~# '^.*256color$'
    set termguicolors
endif
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif


" Configure the Vim status line
" Left:  [VCS Branch][File name][Modified][Read-only][Help][Preview]
"        [Block 1   ][Block 2                                      ]
" Right: [File format][Encoding][File type][Position in file][Column number]
"        [Block 3              ][Block 4  ][Block 5                        ]

" Fetch the VCS branch  TODO add Mercurial support
function! GetVCSBranch() abort
    if vivid#enabled('vim-gitbranch') && gitbranch#name() !=# ''
        return '  ' . gitbranch#name() . ' '
    else | return ''
    endif
endfunction

function! StatusLine(active) abort
    let l:statusline  = "%(%#LineNr#%{GetVCSBranch()}%)"            " Block 1
    let l:statusline .= "%(%#" . a:active . "#\ %f%m%r%h%w\ %)"     " Block 2
    let l:statusline .= "%#"   . a:active . "#%=%<"                 " Right side
    let l:statusline .= "%(%#" . a:active . "#%{&fileformat}\ \ " .
                \ "%{&fileencoding?&fileencoding:&encoding}\ %)"    " Block 3
    let l:statusline .= "%(%#" . a:active . "#%{&filetype==''?'':" .
                \ "'\ '.&ft.'\ '}%)"                                " Block 4
    let l:statusline .= "%(%#" . a:active . "#\ %P\ \ %2c\ %)"      " Block 5
    return l:statusline
endfunction

augroup theming
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine('StatusLine')
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine('StatusLineNC')
    autocmd ColorScheme space-vim-dark highlight SpellBad   ctermbg=NONE
    autocmd ColorScheme space-vim-dark highlight SpellLocal ctermbg=NONE
augroup END

colorscheme space-vim-dark

