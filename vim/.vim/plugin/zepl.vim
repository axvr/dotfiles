" =============================================================
" Description:  Zepl.vim extensions
" File:         plugin/zepl.vim
" =============================================================

packadd zepl
runtime zepl/contrib/load_files.vim

command! -bar -nargs=0 ReplClear :call zepl#send('<C-l>', 1)
nnoremap gz<C-l> :ReplClear<CR>

" command! -bar -nargs=0 ReplClose :call zepl#send('<C-d>', 1)
" nnoremap gz<C-d> :ReplClose<CR>
