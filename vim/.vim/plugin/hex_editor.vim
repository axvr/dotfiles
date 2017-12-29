" =============================================================
" Description:  Allow converting Vim into a hex-editor
" File:         ~/.vim/plugin/hex_editor.vim
" =============================================================

" For more powerful hex-editing use `bvi` or `bless`.

command! -nargs=0 -bar HexModeEnable call <SID>HexModeEnable()
command! -nargs=0 -bar HexModeDisable call <SID>HexModeDisable()

" TODO Possibly create a toggle HexMode

function! s:HexModeEnable() abort
    setlocal binary
    %!xxd
    setlocal filetype=xxd
endfunction

function! s:HexModeDisable() abort
    setlocal binary
    %!xxd -r
    setlocal filetype=
endfunction

nnoremap <Plug>HexRead  :call <SID>HexModeEnable()<CR>
nnoremap <Plug>HexWrite :call <SID>HexModeDisable()<CR>

if empty(maparg('<Leader>hr', 'n'))
    nmap <Leader>hr <Plug>HexRead
endif
if empty(maparg('<Leader>hw', 'n'))
    nmap <Leader>hw <Plug>HexWrite
endif

