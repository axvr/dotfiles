" =============================================================
" Description:  .NET Core CLI (dotnet-cli) Compiler Interface
" File:         ~/.vim/compiler/dotnet.vim
" =============================================================

let current_compiler = 'dotnet'
let s:cpo_save = &cpo
set cpo&vim

if executable('dotnet')
    setlocal makeprg=dotnet\ build\ -v\ q\ .\ /nologo\ /p:GenerateFullPaths=true
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
    "setlocal keywordprg=
endif

let &cpo = s:cpo_save
unlet s:cpo_save
