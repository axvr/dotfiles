" =============================================================
" Description:  Set up Vim for Editing C# *.csproj Files
" File:         ~/.vim/ftdetect/csproj.vim
" =============================================================

autocmd BufNewFile,BufRead *.csproj setlocal sw=2 sts=2 textwidth=0 filetype=xml

