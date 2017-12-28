" =============================================================
" Description:  Easily convert between file formats in Vim
" File:         ~/.vim/plugin/convert_file_format.vim
" =============================================================

function! s:Convert() abort
    let l:ff = &fileformat
    update
    edit ++fileformat=dos
    if l:ff !=# 'unix'
        setlocal fileformat=unix
    endif
    write | edit
endfunction

command! -bar -nargs=0 ConvertFileType :call <SID>Convert()

