" =============================================================
" Description:  Allow converting Vim into a hex-editor
" File:         ~/.vim/plugin/hex_editor.vim
" =============================================================

" TODO Make sure that 'binary' is actually set
" * HexMode is required to read bitcoin blocks
" * HexMode improve - try to stop data corruption
" * HexMode <-- only command to toggle on and off
" * HexMode make unmodifiable

" For more powerful hex-editing use `bvi` or `bless`.
if executable('xxd')
    command! -nargs=0 -bar HexModeEnable call <SID>HexModeEnable()
    command! -nargs=0 -bar HexModeDisable call <SID>HexModeDisable()

    function! s:HexModeEnable() abort
        setlocal binary
        "setlocal textwidth=0
        "setlocal nomodeline
        "setlocal noexpandtab
        "setlocal nomodifiable
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
endif

