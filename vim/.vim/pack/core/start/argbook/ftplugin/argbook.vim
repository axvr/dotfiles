set buftype=acwrite nobuflisted noswapfile hidden

augroup argbook_buffer
    autocmd! BufWriteCmd <buffer>  call argbook#write()
    autocmd! BufReadCmd  <buffer>  call argbook#load()
    autocmd! BufEnter    <buffer>  call argbook#reload()
    " TODO: update after running :argadd/argedit commands while in the argbook buffer.
augroup END

" TODO: change selected arglist entry using :argument num instead of gf
" nnoremap <silent> <buffer> <CR> gf
nnoremap <silent> <buffer> <CR> :call argbook#jump(getbufline(bufnr('%'), line('.'))[0])<CR>

" TODO: q to close/go back.
nnoremap <buffer> q <C-^>

" FIXME: ensure :wall doesn't save this file while hidden.
