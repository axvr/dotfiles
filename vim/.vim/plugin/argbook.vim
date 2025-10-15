" Argbook: Arglists as file bookmarks.  Save/restore/edit/navigate arglists.

" - Something like <https://github.com/idbrii/vim-argedit/blob/master/doc/argedit.txt>
" - Save/restore arglists?
" - Dirvish has arglist integration.
" - Syntax highlighting?  Render as dirvish file?
" - can use ## in Ex-commands to get arglist files for a command.  E.g. :Todos ##
"   - :help :_##

command! -bar -nargs=0 Argbook call argbook#Open()

nnoremap <leader>ab :Argbook<CR>
nnoremap <leader>ac :argument<CR>
nnoremap <leader>aa :$argadd %<CR>
nnoremap <leader>ad :argdelete %<CR>
nnoremap <leader>au :argdedupe<CR>

nmap _ <leader>ab

" Use <C-g> to see if file is in the arglist and where it is in it.
" See: :help arglist-position

" TODO: make optional.
" if has('nvim')
"     augroup argbook_arglocal
"         autocmd!
"         autocmd TabNewEntered * arglocal | %argdelete
"     augroup END
" endif
