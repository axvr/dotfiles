" Left:  [File name][Modified?][RO?][Help?][Preview?]
"        [Block 1  ][Block 2                        ]
" Right: [Line][Column][Percentage][Encoding?]
"        [Block 3     ][Block 4   ][Block 5  ]

function! StatusLineFileEncoding()
    return &fenc =~? '^\(\|utf-8\)$' ? '' : '  ' . &fenc
endfunction

set ruler laststatus=2
set rulerformat=%16(%=%(%{!&nu?line('.').':':''}%c%)%(\ \ %P%)%(%{StatusLineFileEncoding()}%)%<%)

function! StatusLine(active)
    return "%<%(%#".a:active."#\ %f\ %m%r%h%w\ %)%#".a:active."#%=".&rulerformat."\ "
endfunction

augroup set_statusline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine('StatusLine')
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine('StatusLineNC')
augroup END
