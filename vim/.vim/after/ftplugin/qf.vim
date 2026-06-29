" `<C-o>` and `<C-i>`/`<Tab>` switching buffers in the quickfix window makes
" no sense.  Instead make it move between older and newer quickfix lists.
nmap <buffer> <C-o> <Plug>(qf_older)
nmap <buffer> <C-i> <Plug>(qf_newer)

nmap <buffer> { <Plug>(qf_previous_file)
nmap <buffer> } <Plug>(qf_next_file)
