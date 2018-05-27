" =============================================================
" Description:  Set up Vim for Editing TypeScript files
" File:         ~/.vim/ftdetect/typescript.vim
" =============================================================

" TODO maybe replace tsuquyomi with vim-lsp (because of speed issues)
autocmd FileType typescript call vivid#enable('typescript-vim', 'tsuquyomi')
autocmd BufRead,BufNewFile *.ts setlocal filetype=typescript

" TODO Angular Skeletons
"autocmd BufNewFile *.component.ts   %d|r ~/.vim/skeleton/skel.component.ts|1d
