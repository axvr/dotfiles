" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

if executable('dotnet')
    setlocal makeprg=dotnet\ build\ .
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
endif
