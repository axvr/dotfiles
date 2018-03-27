" =============================================================
" Description:  Easily convert between file formats in Vim
" File:         ~/.vim/plugin/convert_file_format.vim
" =============================================================

" Unix --> Dos, Dos --> Unix, Mac --> Unix
function! s:Convert() abort
    let l:ff = &fileformat
    update
    edit ++fileformat=dos
    if l:ff !=# 'unix'
        setlocal fileformat=unix
    endif
endfunction

command! -bar -nargs=0 ConvertFileFormat :call <SID>Convert()

