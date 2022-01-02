" Add custom text objects.
" <https://www.reddit.com/r/vim/comments/8iwuyq/substituting_vimsurround_snippet_plugins_with/dyvz96k/>
for s:char in ['_', '.', ':', ',', ';', '<bar>', '/', '<bslash>', '*', '+', '-', '%', '`']
    execute 'xnoremap i' . s:char . ' :<C-u>normal! T' . s:char . 'vt' . s:char . '<CR>'
    execute 'onoremap i' . s:char . ' :normal vi'      . s:char . '<CR>'
    execute 'xnoremap a' . s:char . ' :<C-u>normal! F' . s:char . 'vf' . s:char . '<CR>'
    execute 'onoremap a' . s:char . ' :normal va'      . s:char . '<CR>'
endfor
