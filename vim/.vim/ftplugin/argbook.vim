set buftype=acwrite nobuflisted noswapfile hidden

augroup argbook_buffer
    autocmd! FileWriteCmd <buffer> call argbook#Write2()
    autocmd! BufWriteCmd  <buffer> call argbook#Write()
    autocmd! BufReadCmd   <buffer> call argbook#Load()
    autocmd! BufEnter     <buffer> call argbook#Reload()
    " TODO: update after running :argadd/argedit commands while the argbook buffer is visible.
    " TODO: define FileReadCmd ???
augroup END

nnoremap <silent> <buffer> <CR> :call argbook#Jump(getbufline(bufnr('%'), line('.'))[0])<CR>
