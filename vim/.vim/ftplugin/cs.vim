" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

" Setup dotnet build as the C# compiler for Vim Quickfix
if executable('dotnet')
    setlocal makeprg=dotnet\ build\ .
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
endif

" TODO find a better solution for syntax file loading with Vivid
if g:loaded_vivid == 1 && vivid#enabled('vim-csharp') != 1
    call vivid#enable('vim-csharp') | edit
endif

" TODO setup OmniSharp

