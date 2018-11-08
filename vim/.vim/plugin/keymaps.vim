" =============================================================
" Description:  Set custom keymaps & commands for Vim
" File:         ~/.vim/plugin/keymaps.vim
" =============================================================

" Spell check toggle
nnoremap <Leader>ss :<C-u>setlocal spell!<CR>
" Allow quick changing of termguicolors
nnoremap <Leader>tc :<C-u>set termguicolors!<CR>
" Toggle the Undotree
nnoremap <Leader>ut :<C-u>UndotreeToggle<CR>
" Display additional file information
nnoremap <Leader>fi :<C-u>echo &fenc?&fenc:&enc '' &ff '' &ft<CR>
" Display current VCS branch
nnoremap <Leader>gb :<C-u>echo system('vcs -b')<CR>

" Add custom text objects
" https://www.reddit.com/r/vim/comments/8iwuyq/substituting_vimsurround_snippet_plugins_with/dyvz96k/
for s:char in [ '_', '.', ':', ',', ';', '<bar>', '/', '<bslash>' ]
    execute 'xnoremap i'.s:char.' :<C-u>normal! T'.s:char.'vt'.s:char.'<CR>'
    execute 'onoremap i'.s:char.' :normal vi'.s:char.'<CR>'
    execute 'xnoremap a'.s:char.' :<C-u>normal! F'.s:char.'vf'.s:char.'<CR>'
    execute 'onoremap a'.s:char.' :normal va'.s:char.'<CR>'
endfor

" Easily convert file formats
" Unix --> Dos, Dos --> Unix, Mac --> Unix
function! s:Convert() abort
    let l:ff = &fileformat
    update
    edit ++fileformat=dos
    if l:ff !=# 'unix'
        setlocal fileformat=unix
    endif
endfunction
command! -bar -nargs=0 ConvertFileFormat :call <SID>Convert()

" Find all TODOs in current repository
function! s:TODOs()
    let l:old_grepprg = &l:grepprg
    setlocal grepprg=todos
    silent grep
    let &l:grepprg = l:old_grepprg
endfunction
command! -nargs=0 -bar TODOs :call <SID>TODOs()
