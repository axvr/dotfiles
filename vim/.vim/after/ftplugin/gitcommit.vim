setlocal iskeyword+=-

" Jump by file.
nnoremap <buffer> [[ zkzakjzaz<CR>
nnoremap <buffer> ]] zjz<CR>

" Jump by hunk.
nmap <silent> <buffer> ( :call apart#DoTimes(v:count1, {-> search('^@@ ', 'Wzb')})<CR>z<CR>4<C-y>
nmap <silent> <buffer> ) :call apart#DoTimes(v:count1, {-> search('^@@ ', 'Wz')})<CR>z<CR>4<C-y>
