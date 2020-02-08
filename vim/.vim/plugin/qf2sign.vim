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

    if !exists('b:qf2s_next_sign_id')
        let b:qf2s_next_sign_id = 1024
    endif

    for i in getqflist()
        if i.lnum == ''
            continue
        endif

        if i.bufnr == l:bufnr
            if i.type ==? 'e'
                let l:type = 'QfError'
            elseif i.type ==? 'w'
                let l:type = 'QfWarning'
            else
                let l:type = 'QfOther'
            endif

            exec 'sign place '.b:qf2s_next_sign_id.' line='.i.lnum.' name='.l:type.' buffer='.l:bufnr

            call insert(l:signs, b:qf2s_next_sign_id)
            let b:qf2s_next_sign_id = b:qf2s_next_sign_id + 1
        endif
    endfor

    " Remove old signs after placing new signs.  This avoids the sign coloum
    " closing and reopening instantly, causing a screen flash.
    if exists('b:signs')
        for i in b:signs
            exec 'sign unplace '.i.' buffer='.l:bufnr
        endfor
    endif

    let b:signs = l:signs
endfunction
