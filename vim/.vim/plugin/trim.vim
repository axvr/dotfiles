" =============================================================
" Description:  Trim Trailing Whitespace
" File:         ~/.vim/plugin/trim.vim
" =============================================================

" Remove trailing whitespace
function! s:trim() abort
    normal! mz
    normal! Hmy
    %s/\m\C\(\\\)\@<!\s\+$//e
    normal! 'yz<CR>
    normal! `z
endfunction

command! -nargs=0 -bar Trim call <SID>trim()

nnoremap <silent> <Plug>Trim :<C-u>call <SID>trim()<CR>

if empty(maparg('<Leader>tw', 'n'))
    nmap <Leader>tw <Plug>Trim
endif
