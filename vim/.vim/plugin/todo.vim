" =============================================================
" Description:  List all TODOs in a project
" File:         ~/.vim/plugin/todo.vim
" =============================================================

" BUG    - A known bug that should be corrected.
" FIXME  - Should be corrected.
" HACK   - A workaround.
" TODO   - Something to be done.
" UNDONE - A reversal or 'roll back' of previous code.
" XXX    - Warn other programmers of problematic or misguiding code.

" TODO work with other VCSs and on single files (maybe search TODO & NOTE files)
function! s:TODOs()
    for l:item in ['TO[-_ ]\?DO', 'FIX[-_ ]\?ME', 'NOTE', 'XXX', 'BUG', 'HACK', 'UNDONE']
        execute 'silent grepadd "'.l:item.'" $(git ls-files)'
    endfor
    redraw!
endfunction
command! -nargs=0 -bar TODOs :call <SID>TODOs()
