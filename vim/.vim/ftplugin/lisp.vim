let b:repl_config = { 'cmd': 'rlwrap sbcl' }

command! -buffer -bar -nargs=1 Describe :call zepl#send("(describe '" . substitute(<q-args>, '\', '', 'g') . ')')

setlocal keywordprg=:Describe

" Use function keys to control the debugger.
nnoremap <silent> <F1> :<C-u>call zepl#send('1')<CR>
nnoremap <silent> <F2> :<C-u>call zepl#send('2')<CR>
nnoremap <silent> <F3> :<C-u>call zepl#send('3')<CR>
nnoremap <silent> <F4> :<C-u>call zepl#send('4')<CR>
nnoremap <silent> <F5> :<C-u>call zepl#send('5')<CR>
nnoremap <silent> <F6> :<C-u>call zepl#send('6')<CR>
nnoremap <silent> <F7> :<C-u>call zepl#send('7')<CR>
nnoremap <silent> <F8> :<C-u>call zepl#send('8')<CR>
nnoremap <silent> <F9> :<C-u>call zepl#send('9')<CR>
nnoremap <silent> <F10> :<C-u>call zepl#send('10')<CR>
nnoremap <silent> <F11> :<C-u>call zepl#send('11')<CR>
nnoremap <silent> <F12> :<C-u>call zepl#send('0')<CR>
