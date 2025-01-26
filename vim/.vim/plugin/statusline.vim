" Left:  [Git branch][File name][Modified][Read-only][Help][Preview]
"        [Block 1   ][Block 2                                      ]
" Right: [Encoding][File type][Position in file][Column number]
"        [Block 3 ][Block 4  ][Block 5                        ]

set ruler laststatus=2
set rulerformat=%32(%=%(%{&fenc?&fenc:&enc}\ %)%(%{&ft==''?'':'\ '.&ft.'\ '}%)%(\ %P\ \ %6{!&nu?line('.').'\ :\ ':''}%2c%)%)

function! s:GitBranch()
    return systemlist('git branch --contains HEAD 2> /dev/null')
                \->get(0, '')->substitute('\m^\*', '', '')->trim()
endfunction

function! StatusLineGitBranch()
    return get(b:, 'git_branch', '') ==# '' ? '' : '  '.b:git_branch.' '
endfunction

function! StatusLine(active)
    return "%(%#DiffAdd#%{&co>85?StatusLineGitBranch():''}%)".
                \ "%(%#".a:active."#\ %f\ %m%r%h%w\ %)".
                \ "%#".a:active."#%=".&rulerformat."\ "
endfunction

augroup set_statusline
    autocmd!
    if has('unix') && executable('git')
        autocmd BufEnter,DirChanged,ShellCmdPost * let b:git_branch = s:GitBranch()
    endif
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine('StatusLine')
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine('StatusLineNC')
augroup END