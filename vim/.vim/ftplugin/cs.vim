" =============================================================
" Description:  Set up Vim for Editing C# (.NET Core) Projects
" File:         ~/.vim/ftplugin/cs.vim
" =============================================================

setlocal foldmarker={,}
setlocal foldmethod=marker
setlocal foldlevel=99

if executable('dotnet')
    setlocal makeprg=dotnet\ build\ .
    setlocal errorformat=\ %#%f(%l\\\,%c):\ %m
endif


" TODO FIXME inprove this
function! s:DotnetRun(...) abort
    echo "0"
    if exists('*.csproj')
        echo "1"
        "system(dotnet run .)
        !dotnet run .
    endif
    echo "2"
endfunction

command! -bar -nargs=? Run :call <SID>DotnetRun(<f-args>)

