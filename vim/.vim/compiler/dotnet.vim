" =============================================================
" Description:  .NET Core CLI (dotnet-cli) Compiler Interface
" File:         ~/.vim/compiler/dotnet.vim
" =============================================================

let s:cpo_save = &cpo
set cpo&vim

if executable('dotnet')
    let current_compiler = 'dotnet'
    let s:make = 'dotnet\ build\ -v\ q\ .\ /nologo\ /p:GenerateFullPaths=true'
    if has('unix') && executable('grep') && executable('sort')
                \ && executable('uniq')
        execute 'setlocal makeprg=' . s:make .
                    \ '\ \\\|\ sort\ \\\|\ uniq\ \\\|\ grep\ \"^/\"'
    else
        execute 'setlocal makeprg=' . s:make
    endif
    setlocal errorformat=%f(%l\\\,%v):\ %t%*[^:]:%m
elseif executable('msbuild')
    compiler msbuild
endif

let &cpo = s:cpo_save
unlet s:cpo_save
