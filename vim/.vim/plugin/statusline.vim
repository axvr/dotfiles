" Improved Vim status line.
"
" The left side displays file information, the right shows location info.
"
" Left:  [File name][+][RO][Help][Preview][Format][Enc]
"        [Block 1  ][Block 2                          ]
" Right: [Line][Column][Percentage]
"        [Block 3     ][Block 4   ]

function! StatusLineFileEncoding()
    " Only display if not UTF-8 encoding.
    return &fenc =~? '^\(\|utf-8\)$' ? '' : '['.&fenc.']'
endfunction

function! StatusLineFileFormat()
    " Only display if not Unix format.
    return &ff ==# 'unix' ? '' : '['.&ff.']'
endfunction

set ruler laststatus=2
set rulerformat=%14(%=%(%{!&nu?line('.').':':''}%c%)%(\ \ %P%)%<%)

function! StatusLine(active)
    let hl = a:active ? 'StatusLine' : 'StatusLineNc'
    return "%#".hl."#%<"
        \ ."%(\ %f\ %)"
        \ ."%(%m%r%h%w%{StatusLineFileFormat()}%{StatusLineFileEncoding()}\ %)"
        \ ."%=".&rulerformat."\ "
endfunction

augroup set_statusline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine(1)
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine(0)
augroup END
