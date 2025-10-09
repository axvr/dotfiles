" Argbook: Arglists as file bookmarks.  Save/restore/edit/navigate arglists.

" - Something like <https://github.com/idbrii/vim-argedit/blob/master/doc/argedit.txt>
" - Press enter to open file.
" - Shortcut to bookmark a file?
" - Command/binding to jump back to current arglist entry.
" - Save/restore arglists.
" - Dirvish has arglist integration.
" - Like harpoon but simpler
" - Write as normal file and it'll update the arglist.
" - Render as dirvish file?

" TODO: <plug>
" TODO: commands

nnoremap <silent> _ :call argbook#open()<CR>
nnoremap gA :argument<CR>

" TODO: make optional.
if has('nvim')
    augroup argbook_arglocal
        autocmd!
        autocmd TabNewEntered * arglocal | %argdelete
    augroup END
endif

" Global mappings and commands:
" _            | View/edit local arglist.
" gA           | Jump to the current arglist file.
" gX           | Add file to local arglist.
" :Args        | Same as _

" Maybe commands:  (prob not needed as you could just `:w ...` and `:r ...`)
" :ArgsSave n | Save local arglist to named file.
" :ArgsLoad n | Load local arglist from named file.

" Argbook file mappings:
" <CR>         | Change arglist index to this file and/or open this file.
" :w           | Apply arglist changes.
