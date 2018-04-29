" =============================================================
" Description:  Configure Styling and Colours for Vim & GVim
" File:         ~/.vim/plugin/styling.vim
" =============================================================

set number relativenumber
set showcmd
set ruler
set rulerformat=%30(%=%(%{&ff}\ \ %{&fenc?&fenc:&enc}\ %)%<%(%{&ft==''?'':'\ '.&ft.'\ '}%)%(\ %P\ \ %2c%)%)
set cursorline
let &colorcolumn='+'.join(range(1,256), ',+')
set belloff=all

if &term =~# '^.*256color$'
    set termguicolors
endif
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif


" Fetch the VCS branch  TODO add support for other VCSs
" TODO improve
function! GetVCSBranch()
    if vivid#enabled('vim-fugitive') && fugitive#head() !=# ''
        return '  ' . fugitive#head() . ' '
    else | return ''
    endif
endfunction

" Left:  [VCS Branch][File name][Modified][Read-only][Help][Preview]
"        [Block 1   ][Block 2                                      ]
" Right: [File format][Encoding][File type][Position in file][Column number]
"        [Block 3              ][Block 4  ][Block 5                        ]

function! StatusLine(active)
    return "%(%#LineNr#%{GetVCSBranch()}%)".
                \ "%(%#".a:active."#\ %f%m%r%h%w\ %)".
                \ "%#".a:active."#%=%<".&rulerformat."\ "
endfunction

augroup theming
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine('StatusLine')
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine('StatusLineNC')
    autocmd ColorScheme space-vim-dark highlight SpellBad   ctermbg=NONE
    autocmd ColorScheme space-vim-dark highlight SpellLocal ctermbg=NONE
augroup END

colorscheme space-vim-dark
