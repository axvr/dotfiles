" =============================================================
" Description:  .NET Core CLI (dotnet-cli) Compiler Interface
" File:         ~/.vim/compiler/dotnet.vim
" =============================================================

let s:cpo_save = &cpo
set cpo&vim

if executable('dotnet')
    let current_compiler = 'dotnet'

    if has('win32') && executable('winpty')
        let s:compiler = "winpty dotnet"
    else
        let s:compiler = "dotnet"
    endif

    let s:make = s:compiler . "\ build\ -v\ q\ .\ /nologo\ /p:GenerateFullPaths=true"

    if has('unix')
        let s:make = s:make . "\ \\\|\ grep\ \"^/\"\ \\\|\ sort\ \\\|\ uniq"
    endif

    " TODO fix file paths for Cygwin (e.g. 'C:\' --> '/c/')
    if has('unix') && has('win32')
        let s:make = s:make . "\ \\\|\ sed\ 's/^C:\\\\/\\/c\\//'"
    endif

    let &l:makeprg = s:make

    setlocal errorformat=%f(%l\\\,%v):\ %t%*[^:]:%m
elseif executable('msbuild')
    compiler msbuild
endif

let &cpo = s:cpo_save
unlet s:cpo_save
