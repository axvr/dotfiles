" Description:  .NET Core compiler integration.
" File:         compiler/dotnet.vim

let s:cpo_save = &cpo
set cpo&vim

let current_compiler = 'dotnet'
let s:make = "dotnet\ build\ $*\ /clp:NoSummary\ /v:q\ /nologo\ /p:GenerateFullPaths=true"

" Fix file paths for Cygwin (e.g. "C:\" --> "/cygdrive/c/")
if has('win32unix')
    let s:make = s:make . "\ \\\|\ tr\ '\\\\' '/' \\\|\ sed\ 's/^\\([A-Z]\\):\\//\\/cygdrive\\/\\1\\//'"
endif

" Remove garbage from the end of the compiler messages
if has('unix')
    let s:make = s:make . "\ \\\|\ sed\ 's/\\s*\\[.*\\?\\]\\s*$//'"
endif

" TODO trim the current working directory from the start of the paths
" - Allows for longer error messages without having to scroll horzontally
" - Fixes interaction with git (using "%" and "#" in ex commands)

let &l:makeprg = s:make

setlocal errorformat=%f(%l\\\,%v):\ %t%*[^:]:%m
setlocal errorformat+=%f\ :\ %t%*[^:]:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save
