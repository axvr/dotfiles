" =============================================================
" Description:  Plugin to Auto-expand Brackets and Pairs
" File:         ~/.vim/plugin/expand_pairs.vim
" =============================================================

" Auto-expanding for closing of pairs
inoremap (; (<CR>);<C-c>O
inoremap (, (<CR>),<C-c>O
inoremap {; {<CR>};<C-c>O
inoremap {, {<CR>},<C-c>O
inoremap [; [<CR>];<C-c>O
inoremap [, [<CR>],<C-c>O

" TODO improve this
function! s:CloseBracket()
    let s:line = getline('.')
    if s:line =~# '^\s*\(struct\|class\|enum\) '
        return "{\<CR>};\<Esc>O"
    elseif searchpair('(', '', ')', 'bmn', '', line('.'))
        " Probably inside a function call. Close it off.
        return "{\<CR>});\<Esc>O"
    else
        return "{\<CR>}\<Esc>O"
    endif
endfunction

inoremap <expr> {<CR> <SID>CloseBracket()

