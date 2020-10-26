" Configure installed packages.

let g:netrw_banner = 0
let g:c_syntax_for_h = 1
let g:org_clean_folds = 1

packadd matchit
packadd cfilter
packadd commentary
packadd traces
packadd lion

packadd cool
let g:CoolTotalMatches = 1

packadd zepl
runtime zepl/contrib/load_files.vim

command! -bar -nargs=0 ReplClear :call zepl#send('<C-l>', 1)
nnoremap gz<C-l> :ReplClear<CR>
