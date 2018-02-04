" =============================================================
" Description:  Allow converting Vim into a hex-editor
" File:         ~/.vim/plugin/hex_editor.vim
" =============================================================

" For more powerful hex-editing use `bvi` or `bless`.
if executable('xxd')

    function! s:ConvertLineEndings(ff) abort
        if &l:ff != a:ff
            try
                ConvertFileFormat
                write | edit
            catch /^Vim\%((\a\+)\)\=:E492/
                throw 'Failed to enable HexMode: Wrong file format'
            catch /^Vim(write):/
                throw 'Failed to enable HexMode: Could not change file format'
            endtry
        endif
    endfunction

    function! s:HexModeEnable() abort
        let s:prev_info = [&tw, &wm, &ml, &et, &ma, &ro, &bin, &ft, &ff]
        call <SID>ConvertLineEndings('unix')
        %!xxd
        setlocal tw=0 wm=0 noml noet noma ro binary ft=xxd
        return 'Enabled HexMode'
    endfunction

    function! s:HexModeDisable() abort
        let &l:tw  = s:prev_info[0] | let &l:wm = s:prev_info[1]
        let &l:ml  = s:prev_info[2] | let &l:et = s:prev_info[3]
        let &l:ma  = s:prev_info[4] | let &l:ro = s:prev_info[5]
        let &l:bin = s:prev_info[6] | let &l:ft = s:prev_info[7]
        let &l:ff  = s:prev_info[8]
        %!xxd -r
        call <SID>ConvertLineEndings(s:prev_info[8])
        write | edit
        unlet s:prev_info
        return 'Disabled HexMode'
    endfunction

    let s:HexModeState = 0

    function! s:HexModeToggle() abort
        if s:HexModeState == 1
            echomsg <SID>HexModeDisable()
            let s:HexModeState = 0
        elseif s:HexModeState == 0
            echomsg <SID>HexModeEnable()
            let s:HexModeState = 1
        endif
    endfunction

    command! -nargs=0 -bar HexMode call <SID>HexModeToggle()

endif

