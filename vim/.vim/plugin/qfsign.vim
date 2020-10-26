" Description:  Show quickfix items in the sign column.
" File:         plugin/qfsign.vim
" Last Updated: 2020-10-24  (created: 2018-07-19)
" Legal:        Public domain.  No rights reserved.

sign define QfWarning text=> texthl=WarningMsg
sign define QfError text=> texthl=ErrorMsg
sign define QfOther text=>

augroup qfsign
    autocmd!
    autocmd BufEnter,QuickFixCmdPost * call s:update_signs()
augroup END

function! s:update_signs()
    let bufnr = bufnr('%')
    let signs = []

    if !exists('b:qfsign_next_id')
        let b:qfsign_next_id = 1024
    endif

    for i in getqflist()
        if i.lnum == ''
            continue
        endif

        if i.bufnr == bufnr
            if i.type ==? 'e'
                let type = 'QfError'
            elseif i.type ==? 'w'
                let type = 'QfWarning'
            else
                let type = 'QfOther'
            endif

            exec 'sign place '.b:qfsign_next_id.' line='.i.lnum.' name='.type.' buffer='.bufnr

            call insert(signs, b:qfsign_next_id)
            let b:qfsign_next_id += 1
        endif
    endfor

    " Remove old signs after placing new signs.  This avoids the sign coloum
    " closing and reopening instantly, causing a screen flash.
    if exists('b:qfsigns')
        for i in b:qfsigns
            exec 'sign unplace '.i.' buffer='.bufnr
        endfor
    endif

    let b:qfsigns = signs
endfunction
