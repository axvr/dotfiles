" =============================================================
" Description:  Append Modeline Template to Current File
" File:         ~/.vim/plugin/append_modeline.vim
" =============================================================

" Append (unobtrusive) modeline template after last line in buffer.
function! s:AppendModeline()
    let l:modeline = printf(' vim: set %set ts=%d sts=%d sw=%d tw=%d '
                \ . 'ft=%s ff=%s fenc=%s :',
                \ &et?'':'no', &ts, &sts, &sw, &tw, &ft, &ff, &fenc)
    " Use substitute() instead of printf() to handle '%%s' in LaTeX files.
    let l:modeline = substitute(&commentstring, '%s', l:modeline, '')
    call append(line('$'), l:modeline)
endfunction

nnoremap <silent> <Plug>AppendModeline :<C-u>call <SID>AppendModeline()<CR>

if empty(maparg('<Leader>ml', 'n'))
    nmap <Leader>ml <Plug>AppendModeline
endif
