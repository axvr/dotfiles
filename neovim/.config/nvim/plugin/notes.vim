let g:waikiki_wiki_roots = [$NOTES_DIR]
let g:waikiki_wiki_patterns = ['/doc/', '/docs/', '/wiki/']
let g:waikiki_default_maps = 1
let g:waikiki_done = 'x'
let g:waikiki_space_replacement = ' '

packadd waikiki

augroup Waikiki
    autocmd!
    autocmd User setup nmap <buffer> <2-LeftMouse> <Plug>(waikikiFollowLink)
    autocmd User setup nnoremap <buffer> gf <Plug>(waikikiFollowLink)
augroup END

function! s:JournalEntry() abort
    let l:date = trim(system(['date', '+%Y-%m-%d']))
    exec 'tabe' $NOTES_DIR . '/Journal/' . l:date . '.md'
endfunction

command -nargs=0 Notes tabe ~/Documents/Notes
command -nargs=0 Journal call s:JournalEntry()
