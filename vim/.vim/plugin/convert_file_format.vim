" =============================================================
" Description:  Easily convert between file formats in Vim
" File:         ~/.vim/plugin/convert_file_format.vim
" =============================================================

function! ConvertTo(fileformat) abort
    let l:fileformats = substitute(&fileformats, '^', ',', '')
    let l:fileformats = substitute(l:fileformats, '$', ',', '')
    if l:fileformats =~? ',' . a:fileformat . ','
        if a:fileformat ==? 'unix'
            call s:unix()
        elseif a:fileformat ==? 'dos'
            call s:dos()
        endif
    else
        echo 'Invalid file format'
    endif
endfunction

function! s:unix() abort
    if &fileformat ==? 'dos'
        edit ++fileformat=dos
        setlocal fileformat=unix
    endif
endfunction

function! s:dos() abort
    if &fileformat ==? 'unix'
        edit ++fileformat=dos
    endif
endfunction