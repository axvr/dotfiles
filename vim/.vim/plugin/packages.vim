" Configure installed packages.

let g:netrw_banner = 0
let g:org_clean_folds = 1
let g:CoolTotalMatches = 1

runtime ftplugin/man.vim

packadd matchit
packadd commentary
packadd surround
packadd traces
packadd apart
" packadd cfilter
packadd qf

packadd zepl
runtime zepl/contrib/load_files.vim

command! -bar -nargs=0 ReplClear :call zepl#send('<C-l>', 1)
nnoremap gz<C-l> :ReplClear<CR>

com! -nargs=0 -bar Helptags for p in glob('~/.vim/pack/*/*/*/doc',1,1)|exe 'helpt '.p|endfo
