" Summary: Vim styling.  E.g. colours and statusline config.
" Help:    N/A

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

" Enable blinking cursor.
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
            \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
            \,sm:block-blinkwait175-blinkoff150-blinkon175

" Configure Vim status line.  Left, file info; right, location info.
"
" Left:  ([File name])     ([+][RO][Help][Preview][Format][Enc])
" Right: ([Line] [Column]) ([Percentage])

function! StatusLineFileEncoding()
    " Only display if not UTF-8 encoding.
    return &fenc =~? '^\(\|utf-8\)$' ? '' : '['..&fenc..']'
endfunction

function! StatusLineFileFormat()
    " Only display if not Unix format.
    return &ff ==# 'unix' ? '' : '['..&ff..']'
endfunction

set ruler laststatus=2
set rulerformat=%14(%=%(%{!&nu?line('.')..':':''}%c%)%(\ \ %P%)%<%)

function! StatusLine(active)
    let hl = a:active ? 'StatusLine' : 'StatusLineNc'
    return "%#"..hl.."#%<"
        \ .."%(\ %f\ %)"
        \ .."%(%m%r%h%w%{StatusLineFileFormat()}%{StatusLineFileEncoding()}\ %)"
        \ .."%="..&rulerformat.."\ "
endfunction

augroup axvr/statusline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal statusline=%!StatusLine(1)
    autocmd WinLeave,BufLeave * setlocal statusline=%!StatusLine(0)
augroup END
