" =============================================================
" Description:  Configure Styling and Colours for Vim & GVim
" File:         ~/.vim/config/styling.vim
" =============================================================

scriptencoding utf-8

set number relativenumber
set showmode showcmd
set ruler
set rulerformat=%.20(%=%<%(%{&filetype==''?'':'\ '.&ft.'\ '}%)%(\ %P\ \ %2c%)%)
set cursorline          " Highlight current line
let &colorcolumn='+'.join(range(1,256), ',+')
set visualbell t_vb=    " Disable sound & visual alerts
set laststatus=2        " Always display statusline

if has('gui_running')  " Just incase I ever use GVim (not likely)
    set guifont=Monospace\ 11
    set guioptions-=T guioptions-=m guioptions-=r guioptions+=c guioptions-=L
else
    if $TERM == 'xterm-256color'
        set termguicolors
    endif
endif

function! s:CheckTMUX() abort
    if system('printf "$TMUX"') ==# ''
        highlight Comment cterm=italic
    else
        highlight Comment cterm=NONE
    endif
endfunction

" Configure the Vim status line
" Left:  [VCS Branch][File name][Modified][Read-only][Help][Preview]
"        [Block 1   ][Block 2                                      ]
" Right: [File format][Encoding][File type][Position in file][Column number]
"        [Block 3              ][Block 4  ][Block 5                        ]

" FIXME Fetch the VCS branch 
function! GetVCSBranch() abort
    if executable('parse_vcs_branch') " <-- This doesn't work for some reason
        let s:branch = system('parse_vcs_branch')
    elseif vivid#enabled('vim-fugitive')
        let s:branch = fugitive#head()
    else
        let s:branch = ''
    endif

    if s:branch != ''
        return '  ' . s:branch . ' '
    else | return ''
    endif
endfunction

function! ActiveStatus() abort
    let l:statusline  = "%(%#LineNr#%{GetVCSBranch()}%)"          " Block 1
    let l:statusline .= "%(%#StatusLine#\ %f%m%r%h%w\ %)"         " Block 2
    let l:statusline .= "%#StatusLine#%=%<"                       " Right side
    let l:statusline .= "%(%#StatusLine#%{&fileformat}\ \ " .
                \ "%{&fileencoding?&fileencoding:&encoding}\ %)"  " Block 3
    let l:statusline .= "%(%#StatusLine#%{&filetype==''?'':'\ '.&ft.'\ '}%)"    " Block 4
    let l:statusline .= "%(%#StatusLine#\ %P\ \ %2c\ %)"          " Block 5
    return l:statusline
endfunction

function! InactiveStatus() abort
    let l:statusline  = "%(%#LineNr#%{GetVCSBranch()}%)"          " Block 1
    let l:statusline .= "%(%#StatusLineNC#\ %f%m%r%h%w\ %)"       " Block 2
    let l:statusline .= "%#StatusLineNC#%=%<"                     " Right side
    let l:statusline .= "%(%#StatusLineNC#%{&fileformat}\ \ " .
                \ "%{&fileencoding?&fileencoding:&encoding}\ %)"  " Block 3
    let l:statusline .= "%(%#StatusLineNC#%{&filetype==''?'':'\ '.&ft.'\ '}%)"  " Block 4
    let l:statusline .= "%(%#StatusLineNC#\ %P\ \ %2c\ %)"        " Block 5
    return l:statusline
endfunction

augroup theming
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!ActiveStatus()
    autocmd WinLeave,BufLeave * setlocal statusline=%!InactiveStatus()
    autocmd ColorScheme space-vim-dark highlight SpellBad   ctermbg=NONE
    autocmd ColorScheme space-vim-dark highlight SpellLocal ctermbg=NONE
    autocmd ColorScheme space-vim-dark call <SID>CheckTMUX()
augroup END

colorscheme space-vim-dark

