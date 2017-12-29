" =============================================================
" Description:  Append Custom Modeline to Current File
" File:         ~/.vim/plugin/append_modeline.vim
" =============================================================

" Append modeline after last line in buffer.
function! AppendModeline()
    if exists('g:modeline')
        " Allow setting modeline from another vim config file
        let l:modeline = g:modeline
    else
        " TODO Reduce the size of the modeline
        let l:modeline = printf(' vim: set ts=%d sw=%d tw=%d %set ft=%s ' 
                    \ . 'ff=%s fenc=%s fdm=%s fmr=%s :',
                    \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no',
                    \ &filetype, &fileformat, &fileencoding, 
                    \ &foldmethod, &foldmarker)
    endif
    " Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
    " files.
    let l:modeline = substitute(&commentstring, '%s', l:modeline, '')
    call append(line('$'), l:modeline)
endfunction

nnoremap <silent> <Plug>AppendModeline :<C-u>call AppendModeline()<CR>

if empty(maparg('<Leader>ml', 'n'))
    nmap <Leader>ml <Plug>AppendModeline
endif

