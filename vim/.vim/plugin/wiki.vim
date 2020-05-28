" =============================================================
" Description:  Waikiki configuration
" File:         plugin/wiki.vim
" =============================================================

let g:waikiki_roots = ['~/Documents/Wiki/', '~/Documents/Projects/website/content/']
let g:waikiki_default_maps = 1
packadd waikiki
augroup Waikiki
    autocmd!
    autocmd User setup nmap <buffer> <2-LeftMouse> <Plug>(waikikiFollowLink)
    autocmd BufEnter * silent! delcommand WaikikiTags
augroup END
