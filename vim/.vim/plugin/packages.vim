" Configure packages.

" Regenerate help tags for plugins.
command! -nargs=0 -bar Helptags
            \ for s:dir in glob('~/.vim/pack/*/*/*/doc', 1, 1)
            \ |   execute 'helptags ' . s:dir
            \ | endfor
            \ | unlet s:dir

let g:org_clean_folds = 1
let g:org_use_italics = 1
let g:CoolTotalMatches = 1

runtime ftplugin/man.vim
set keywordprg=:Man

packadd matchit
packadd commentary
packadd traces
packadd apart
packadd qf

" Replace netrw with dirvish.
let g:netrw_banner = 0
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1
packadd dirvish

packadd zepl
runtime zepl/contrib/load_files.vim

command! -bar -nargs=0 ReplClear call zepl#send("\<C-l>", 1)
nnoremap gz<C-l> :ReplClear<CR>
