" =============================================================
" Description:  Trim Trailing Whitespace
" File:         ~/.vim/plugin/trim.vim
" =============================================================

" Remove trailing whitespace
" FIXME don't remove a whitespace character after a backslash
function! s:trim() abort
    normal! mz
    normal! Hmy
    %s/\m\C\s\+$//e
    normal! 'yz<CR>
    normal! `z
endfunction

command! -nargs=0 -bar Trim call <SID>trim()

