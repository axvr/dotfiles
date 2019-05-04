" Waikiki config

let g:waikiki_default_maps = 1
let g:waikiki_wiki_roots = ['~/Documents/Notes', '~/Documents/Projects/website']

augroup Waikiki
    autocmd!
    autocmd User setup call <SID>waikikiMappings()
augroup END

function s:waikikiMappings()
    nmap <buffer> <C-]> <Plug>(waikikiFollowLink)
    nmap <buffer> gf    <Plug>(waikikiFollowLink)
    xmap <buffer> <C-]> <Plug>(waikikiFollowLink)
    xmap <buffer> gf    <Plug>(waikikiFollowLink)
endfunction
