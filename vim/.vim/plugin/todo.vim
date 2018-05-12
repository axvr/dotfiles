" =============================================================
" Description:  List all TODOs in a project
" File:         ~/.vim/plugin/todo.vim
" =============================================================

" TODO work with other VCSs and on single files
function! s:TODOs()
    silent grep    "TODO"  $(git ls-files)
    silent grepadd "FIXME" $(git ls-files)
    silent grepadd "NOTE"  $(git ls-files)
    redraw!
endfunction
command! -nargs=0 -bar TODOs :call <SID>TODOs()
