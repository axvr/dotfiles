" Task management.
com! -nargs=* -complete=file Todos let s:gp=&l:gp|setl gp=todos|gr <args>|let &l:gp=s:gp|unl s:gp
com! -nargs=0 -bar Tasks tabe DONE|sp DOING|sp TODO

" Git integration.
com! -nargs=+ -complete=file GitGrep let s:gp=&l:gp|setl gp=git\ grep\ -n|gr <args>|let &l:gp=s:gp|unl s:gp
com! -nargs=? -range GitBlame ec join(systemlist("git -C ".shellescape(expand('%:p:h')).
            \ " blame -L <line1>,<line2> <args> -- ".expand('%:t')),"\n")

augroup project
    autocmd!
    " When editing a file, jump to the last known cursor position.
    autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " Reset cursor position to line 1 when writing a Git commit/tag message.
    autocmd BufReadPost COMMIT_EDITMSG,TAG_EDITMSG normal! gg
augroup END

" Session restore.
nnoremap <F4> :<C-u>source Session.vim<CR>

" Command to diff unsaved changes to current file.  Deactivate with :diffoff!
command! -nargs=0 -bar DiffOrig botright vertical new
            \ | set buftype=nofile | read ++edit # | 0d_ | diffthis
            \ | wincmd p | diffthis
