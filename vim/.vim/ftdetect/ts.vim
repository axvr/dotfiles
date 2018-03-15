" =============================================================
" Description:  Set up Vim for Editing TypeScript files
" File:         ~/.vim/ftdetect/ts.vim
" =============================================================

augroup TypeScript
    autocmd!
    " FIXME Speed issues with starting the server
    autocmd FileType ts call vivid#enable('typescript-vim', 'tsuquyomi')
    autocmd BufReadPre,BufNewFile *.ts setlocal filetype=ts
augroup END

