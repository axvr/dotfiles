" =============================================================
" Description:  List all TODOs in a project
" File:         ~/.vim/plugin/todos.vim
" =============================================================

function! s:TODOs()
" function! s:TODOs(...)
    let l:old_grepprg = &l:grepprg
    setlocal grepprg=todos
    " join(map(copy(a:000), {k, v -> expand(v)}), '" ')
    silent grep
    let &l:grepprg = l:old_grepprg
endfunction

command! -nargs=0 -bar TODOs :call <SID>TODOs()
" command! -nargs=* -bar TODOs :call <SID>TODOs(<f-args>)
