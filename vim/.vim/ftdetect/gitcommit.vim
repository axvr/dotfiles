" =============================================================
" Description:  Improve Git Commit Message Editing
" File:         ~/.vim/ftdetect/gitcommit.vim
" =============================================================

function! s:configure_committia() abort
    if vivid#enabled('committia.vim') &&
                \ expand('%:t') =~# '\m\C__committia_\(diff\|status\)__'
        setlocal nocursorline colorcolumn=
    endif
endfunction

autocmd BufReadPre COMMIT_EDITMSG call vivid#enable('committia.vim')
autocmd FileType git,gitcommit call <SID>configure_committia()
