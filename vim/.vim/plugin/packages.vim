" Summary: Configure and enable packages.
" Help:    :help packages

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
    \ call glob('~/.vim/pack/*/*/*/doc', 1, 1)
    \ ->add(expand('~/.vim/doc'))
    \ ->foreach("exec '<mods> helptags' v:val")

" Add `:Man` and `:Info`.
runtime ftplugin/man.vim
set keywordprg=:Man
packadd info

packadd qf
let g:qf_number = 0

let g:fugitive_legacy_commands = v:false
packadd fugitive
if !has('nvim')
    autocmd! fugitive TerminalOpen !git* set nobuflisted
endif

packadd argbook
packadd matchit

packadd zepl
runtime zepl/contrib/load_files.vim
if has('nvim') | runtime zepl/contrib/nvim_autoscroll_hack.vim | endif
command! -bar -nargs=0 ReplClear call zepl#send("\<C-l>", 1)
nnoremap gz<C-l> :ReplClear<CR>

" Vim 9.1 has a built-in "comment" package.  Fallback to "commentary".
try | packadd comment | catch | packadd commentary | endtry

if !has('nvim')
    packadd traces
    packadd unimpaired

    " Vim 9.1 adds a built-in "editorconfig" package.
    if filereadable('.editorconfig') | silent! packadd editorconfig | endif
endif
