" Summary: Notes in Vim.
" Help:    :help waikiki.txt

command! -nargs=0 -bar Notes
            \ <mods> tabedit $NOTES_DIR | silent lcd $NOTES_DIR | arglocal

" TODO: replace Waikiki.vim with a simpler link plugin?

let g:waikiki_wiki_roots = [$NOTES_DIR]
let g:waikiki_wiki_patterns = ['/doc/', '/docs/', '/wiki/', '/website/']
let g:waikiki_default_maps = 1
let g:waikiki_done = 'x'
let g:waikiki_ext = '.md'
let g:waikiki_index = 'index.md'
let g:waikiki_space_replacement = ' '
let g:waikiki_conceal_markdown_url = 0

packadd waikiki

function! s:SetUpWaikiki()
    if &l:ft ==# 'markdown'
        nnoremap <unique> <buffer> <2-LeftMouse> <Plug>(waikikiFollowLink)
        nnoremap <unique> <buffer> gf <Plug>(waikikiFollowLink)
        setlocal concealcursor&
    endif
endfunction

augroup Waikiki
    autocmd!
    autocmd Waikiki User setup call s:SetUpWaikiki()
augroup END
