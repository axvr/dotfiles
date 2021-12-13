" Task management.
com! -nargs=* -complete=file Todos let s:gp=&l:gp|setl gp=todos|gr <args>|let &l:gp=s:gp|unl s:gp
com! -nargs=0 -bar Tasks tabe DONE|sp DOING|sp TODO

" Git integration.
com! -nargs=+ -complete=file GitGrep let s:gp=&l:gp|setl gp=git\ grep\ -n|gr <args>|let &l:gp=s:gp|unl s:gp
com! -nargs=? -range GitBlame ec join(systemlist("git -C ".shellescape(expand('%:p:h')).
            \ " blame -L <line1>,<line2> <args> -- ".expand('%:t')),"\n")

" Session restore.
nnoremap <F4> :<C-u>source Session.vim<CR>
