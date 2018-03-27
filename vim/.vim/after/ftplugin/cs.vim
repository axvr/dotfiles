" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/after/ftplugin/cs.vim
" =============================================================

" FIXME Gets overwritten if another C# file is opened
" NOTE: Done in `after` to overwrite `vim-csharp` plugin settings
compiler dotnet
setlocal completeopt& completeopt-=preview

