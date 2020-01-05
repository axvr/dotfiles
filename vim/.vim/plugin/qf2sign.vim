" =============================================================
" Description:  Show quickfix items in the sign column.
" File:         ~/.vim/plugin/qf2sign.vim
" Licence:      Public domain.
" Created:      2018-07-19
" =============================================================

sign define QfWarning text=> texthl=WarningMsg
sign define QfError text=> texthl=ErrorMsg
sign define QfOther text=>

autocmd! BufEnter,QuickFixCmdPost * call <SID>update_signs()

function! s:update_signs()
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

    " Remove old signs after placing the new ones. This reduces screen refresh time.
    if exists('b:signs')
        for i in b:signs
            exec 'sign unplace '.i.' buffer='.l:bufnr
        endfor
    endif

    let b:signs = l:signs
endfunction
