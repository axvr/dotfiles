" =============================================================
" Description:  Set up Vim for Editing TypeScript files
" File:         ~/.vim/ftdetect/typescript.vim
" =============================================================

augroup TypeScript
    autocmd!
    " FIXME Speed issues with starting the server
    " TODO maybe replace tsuquyomi with vim-lsp
    autocmd FileType typescript call vivid#enable('typescript-vim', 'tsuquyomi')
    autocmd BufRead,BufNewFile *.ts setlocal filetype=typescript
augroup END

