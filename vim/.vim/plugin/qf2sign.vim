" =============================================================
" Description:  Show quickfix items in the sign column
" File:         ~/.vim/plugin/qf2sign.vim
" =============================================================

" TODO diff to sign column (use DiffAdd DiffChange, DiffRemove)

sign define QfWarning text=> texthl=WarningMsg
sign define QfError text=> texthl=ErrorMsg
sign define QfOther text=>

autocmd! BufEnter,QuickFixCmdPost * call <SID>main()

" Sometimes the screen doesn't get correctly redrawn
autocmd! QuickFixCmdPost * redraw!

function! s:main()
    let l:bufnr = bufnr('%')
    let l:signs = []

    if !exists('b:itt')
        let b:itt = 1024
    endif

    for i in getqflist()
        if i.lnum == ''
            continue
        endif

        if i.bufnr == l:bufnr
            if i.type ==? 'e'
                let type = 'QfError'
            elseif i.type ==? 'w'
                let type = 'QfWarning'
            else
                let type = 'QfOther'
            endif

            exec 'sign place '.b:itt.' line='.i.lnum.' name='.type.' buffer='.l:bufnr

            call insert(l:signs, b:itt)
            let b:itt = b:itt + 1
        endif
    endfor

    " Remove old signs after placing new ones to avoid screen refresh slow downs
    if exists('b:signs')
        for i in b:signs
            exec 'sign unplace '.i.' buffer='.l:bufnr
        endfor
    endif

    let b:signs = l:signs

endfunction
