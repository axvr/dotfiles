" =============================================================
" Description:  Plugin to Auto-expand Brackets and Pairs
" File:         ~/.vim/plugin/expand_pairs.vim
" =============================================================

" FIXME: Causes issues when pasting from system clipboard

" Auto-close/expand brackets
function! s:CloseBracket()
    let s:line = getline('.')
    if s:line =~# '^\s*\(struct\|enum\) '
        return "{\<CR>};\<Esc>O"
    elseif searchpair('(', '', ')', 'bmn', '', line('.'))
        " Probably inside a function call. Close it off.
        return "{\<CR>});\<Esc>O"
    else
        return "{\<CR>}\<Esc>O"
    endif
endfunction

" inoremap <expr> {<CR> <SID>CloseBracket()
