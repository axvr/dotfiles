" =============================================================
" Description:  Add Additional Text Objects
" File:         ~/.vim/plugin/objects.vim
" =============================================================

" https://www.reddit.com/r/vim/comments/8iwuyq/substituting_vimsurround_snippet_plugins_with/dyvz96k/
for s:char in [ '_', '.', ':', ',', ';', '<bar>', '/', '<bslash>', '*', '+', '%', '`' ]
    execute 'xnoremap i' . s:char . ' :<c-u>normal! T' . s:char . 'vt' . s:char . '<cr>'
    execute 'onoremap i' . s:char . ' :normal vi' . s:char . '<cr>'
    execute 'xnoremap a' . s:char . ' :<c-u>normal! F' . s:char . 'vf' . s:char . '<cr>'
    execute 'onoremap a' . s:char . ' :normal va' . s:char . '<cr>'
endfor
