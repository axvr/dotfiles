" =============================================================
" Description:  Set up Vim for Editing TypeScript files
" File:         ~/.vim/ftdetect/typescript.vim
" =============================================================

autocmd FileType typescript call vivid#enable('typescript-vim')
autocmd BufRead,BufNewFile *.ts setlocal filetype=typescript
