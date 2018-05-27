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

function! s:TODOs()
    let l:vcs = system('vcs-branch -v')
    let l:files = ''
    if l:vcs == 'git'
        let l:files = '$(git ls-files)'
    elseif l:vcs == 'hg'
        let l:files = '$(hg status -An)'
    elseif filereadable(expand('%'))
        let l:files = '%'
    endif

    for l:item in ['TO[-_ ]\?DO', 'FIX[-_ ]\?ME', 'NOTE', 'XXX', 'BUG', 'HACK', 'UNDONE']
        execute 'silent grepadd "'.l:item.'" '.l:files
    endfor
    redraw!
endfunction
command! -nargs=0 -bar TODOs :call <SID>TODOs()
