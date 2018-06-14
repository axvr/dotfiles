" =============================================================
" Description:  Configure Styling and Colours for Vim & GVim
" File:         ~/.vim/plugin/styling.vim
" =============================================================

set number relativenumber
set showcmd
set ruler
set rulerformat=%32(%=%(%{&ff}\ \ %{&fenc?&fenc:&enc}\ %)%<%(%{&ft==''?'':'\ '.&ft.'\ '}%)%(\ %P\ \ %2c%)%)
set cursorline
let &colorcolumn='+'.join(range(1,256), ',+')
set synmaxcol=256
set belloff=all

if &term =~# '^.*256color$'
    set termguicolors
endif
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

" Fetch the VCS branch
if has('unix') && executable('vcs')
    autocmd! BufReadPre,BufWritePost,FileWritePost,DirChanged,ShellCmdPost *
                \ let b:vcs_branch = system('vcs -b')
endif
function! VCSBranch()
    if exists('b:vcs_branch') && b:vcs_branch != ''
        return '  '.b:vcs_branch.' '
    endif | return ''
endfunction

" Left:  [VCS Branch][File name][Modified][Read-only][Help][Preview]
"        [Block 1   ][Block 2                                      ]
" Right: [File format][Encoding][File type][Position in file][Column number]
"        [Block 3              ][Block 4  ][Block 5                        ]

function! StatusLine(active)
    return "%(%#LineNr#%{VCSBranch()}%)".
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
