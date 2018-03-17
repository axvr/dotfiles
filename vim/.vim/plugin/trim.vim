" =============================================================
" Description:  Trim Trailing Whitespace
" File:         ~/.vim/plugin/trim.vim
" =============================================================

" Remove trailing whitespace
function! s:trim(bang) abort
    if a:bang || (!&binary && &filetype != 'diff')
        normal! mz
        normal! Hmy
        %s/\m\C\s\+$//e
        normal! 'yz<CR>
        normal! `z
    else
        echomsg 'Warning! Not reccommended to trim whitespace in this file.'
    endif
endfunction

command! -nargs=0 -bar -bang Trim call <SID>trim('!' == '<bang>')
