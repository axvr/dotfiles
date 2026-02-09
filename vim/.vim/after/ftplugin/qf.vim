" `<C-o>` and `<C-i>`/`<Tab>` switching buffers in the quickfix window makes
" no sense.  Instead make it move between older and newer quickfix lists.
nnoremap <buffer> <C-o> :<C-u>colder<CR>
nnoremap <buffer> <Tab> :<C-u>cnewer<CR>
