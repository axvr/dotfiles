" Description:  Waikiki configuration
" File:         plugin/wiki.vim

let g:waikiki_roots = ['~/Documents/Wiki/']
let g:waikiki_default_maps = 1
let g:waikiki_index = 'Index.org'
let g:waikiki_ext = '.org'

" Roam-style links.
let g:waikiki_link_regex = '\[\[[[:alnum:] _-]\+\]\]'
let g:waikiki_link_url_regex = '\[\[\zs[[:alnum:] _-]\+\ze\]\]'
let g:waikiki_link_fmt = '[[%s%.0s%.0s]]'

packadd waikiki

augroup Waikiki
    autocmd!
    autocmd User setup nmap <buffer> <2-LeftMouse> <Plug>(waikikiFollowLink)
    autocmd user setup silent! delcommand WaikikiTags
    autocmd User setup nnoremap gf <Plug>(waikikiFollowLink)
augroup END

" TODO: choose a wiki page to open.
command -nargs=0 Wiki tabe ~/Documents/Wiki/Index.org
