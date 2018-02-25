" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

" Setup dotnet build as the C# compiler for Vim Quickfix
if executable('dotnet')
    setlocal makeprg=dotnet\ build\ .
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
    "setlocal keywordprg=
endif

" TODO setup OmniSharp

