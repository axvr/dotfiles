function! s:colour_group()
    if exists("*synstack")
        echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
    endif
endfunction

command! -nargs=0 SyntaxGroupUnderCursor :call <SID>colour_group()
