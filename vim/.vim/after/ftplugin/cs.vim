" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/after/ftplugin/cs.vim
" =============================================================

" Setup dotnet build as the C# compiler for Vim Quickfix
" NOTE: Done in `after` to overwrite `vim-csharp` plugin settings
if executable('dotnet')
    setlocal makeprg=dotnet\ build\ .
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
    "setlocal keywordprg=
endif

setlocal completeopt& completeopt-=preview

