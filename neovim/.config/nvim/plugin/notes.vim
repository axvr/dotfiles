let g:waikiki_roots = ['~/Documents/Notes/']
let g:waikiki_default_maps = 1
let g:waikiki_done = 'x'

packadd waikiki

augroup Waikiki
    autocmd!
    autocmd User setup nmap <buffer> <2-LeftMouse> <Plug>(waikikiFollowLink)
    autocmd user setup silent! delcommand WaikikiTags
    autocmd User setup nnoremap gf <Plug>(waikikiFollowLink)
augroup END

command -nargs=0 Notes tabe ~/Documents/Notes
