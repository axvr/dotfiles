" Smarter J mapping for Lisp dev.  Removes extra whitespace before closing brackets.

function! s:J(count) abort
    let c = a:count
    while c > 0
        normal! J
        let nextchar  = getline('.')[getcursorcharpos()[2] - 1]
        let nnextchar = getline('.')[getcursorcharpos()[2]]
        if nextchar ==# ' ' && (nnextchar ==# ']' || nnextchar ==# '}')
            normal! x
        endif
        let c -= 1
    endwhile
endfunction

nnoremap <silent> <Plug>LispyJ :<C-u>call <SID>J(v:count1)<CR>
